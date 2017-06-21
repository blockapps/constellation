{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Constellation.Node.ServantApi where

import           Constellation.Enclave.Payload
import           Constellation.Node.Types
import           Control.Error
import           Control.Monad
import           Control.Monad.Error.Class
import qualified Control.Monad.Ether.Implicit         as I
import           Control.Monad.IO.Class
import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON (toJSON),
                                                       Value (Null, Object),
                                                       object, (.:), (.=))
import           Data.ByteArray.Encoding              (Base (Base64),
                                                       convertFromBase,
                                                       convertToBase)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8
import           Data.Function                        ((&))
import           Data.Proxy
import           Data.Text                            (Text)
import           Network.HTTP.Client                  (newManager)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           Network.HTTP.ReverseProxy
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)


import qualified Data.Text.Encoding                   as TE

import qualified Network.HTTP.Types                   as H

import           Constellation.Enclave.Types          (PublicKey)
import           Constellation.Util.ByteString        (b64BsDecodeText,
                                                       mustB64TextDecodeBs)

import           Servant
import           Servant.Client
import           Servant.Server                       ()

import           System.Environment                   (lookupEnv)

import           Network.Wai.Handler.Warp             (run)

--------------------------------------------------------------------------------
-- * API Types
--------------------------------------------------------------------------------

data SendRequest = SendRequest
    { sreqPayload :: ByteString
    , sreqFrom    :: PublicKey
    , sreqTo      :: [PublicKey]
    } deriving (Eq, Show)

instance FromJSON SendRequest where
    parseJSON (Object v) = SendRequest
        <$> (mustB64TextDecodeBs <$> v .: "payload")
        <*> v .: "from"
        <*> v .: "to"
    parseJSON _          = fail "Could not parse SendRequest"


instance ToJSON SendRequest where
  toJSON sendReq = object [ "payload" .= (b64BsDecodeText . sreqPayload $ sendReq)
                          , "from" .= sreqFrom sendReq
                          , "to" .= sreqTo sendReq
                          ]

data SendResponse = SendResponse
    { sresKey :: Text
    } deriving (Show)

instance ToJSON SendResponse where
    toJSON SendResponse{..} = object
        ["key" .= sresKey]

instance FromJSON SendResponse where
  parseJSON (Object v) = SendResponse <$> v .: "key"
  parseJSON _          = fail "Could not parse SendResponse"

data ReceiveRequest = ReceiveRequest
    { rreqKey :: Text
    , rreqTo  :: PublicKey
    } deriving (Show)

instance FromJSON ReceiveRequest where
    parseJSON (Object v) = ReceiveRequest
        <$> v .: "key"
        <*> v .: "to"
    parseJSON _          = fail "Could not parse ReceiveRequest"

instance ToJSON ReceiveRequest where
  toJSON recReq = object [ "key" .= rreqKey recReq
                         , "to" .= rreqTo recReq
                         ]

data ReceiveResponse = ReceiveResponse
    { rresPayload :: ByteString
    } deriving (Show)

instance ToJSON ReceiveResponse where
    toJSON ReceiveResponse{..} = object
        [ "payload" .= TE.decodeUtf8 (convertToBase Base64 rresPayload)
        ]

instance FromJSON ReceiveResponse where
  parseJSON (Object v) = do
    eRes <- convertFromBase Base64 . TE.encodeUtf8 <$> v .: "payload"
    either (const . fail $ "Could not parse ReceiveResponse") (return . ReceiveResponse) eRes
  parseJSON _ = fail "Could not parse ReceiveResponse"

data DeleteRequest = DeleteRequest
    { dreqKey :: Text
    } deriving (Show)

instance FromJSON DeleteRequest where
    parseJSON (Object v) = DeleteRequest
        <$> v .: "key"
    parseJSON _          = fail "Could not parse DeleteRequest"

instance ToJSON DeleteRequest where
  toJSON delReq = object [ "key" .= dreqKey delReq ]

data Resend = ResendIndividual PublicKey Text
            | ResendAll PublicKey
            deriving (Show)

instance FromJSON Resend where
    parseJSON (Object v) = do
        t <- v .: "type"
        case (t :: Text) of
            "individual" -> ResendIndividual
                <$> v .: "publicKey"
                <*> v .: "key"
            "all"        -> ResendAll <$> v .: "publicKey"
            _            -> mzero
    parseJSON _          = mzero

instance ToJSON Resend where
  toJSON rsnd = case rsnd of
    ResendIndividual pk txt -> object [ "type" .= ("individual" :: String)
                                      , "publicKey" .= pk
                                      , "key" .= txt
                                      ]
    ResendAll pk -> object [ "type" .= ("all" :: String)
                           , "publicKey" .= pk
                           ]

data ResendResponse = ResendIndividualRes EncryptedPayload
                    | ResentAll
                    deriving (Show)

instance ToJSON ResendResponse where
  toJSON rsndResp = case rsndResp of
    ResendIndividualRes epl -> toJSON epl
    ResentAll               -> Null

instance FromJSON ResendResponse where
  parseJSON o@(Object _) = ResendIndividualRes <$> parseJSON o
  parseJSON Null         = return ResentAll
  parseJSON _            = fail "Could not parse ResendResponse."

--------------------------------------------------------------------------------
-- | API
--------------------------------------------------------------------------------

type API = "send" :> ReqBody '[JSON] SendRequest :> Post '[JSON] SendResponse
      :<|> "receive" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "receiveRaw" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "delete" :> ReqBody '[JSON] DeleteRequest :> Post '[JSON] NoContent

type RawSend = "sendRaw" :> Raw

type P2P = "push" :> ReqBody '[JSON] EncryptedPayload :> Post '[JSON] Text
      :<|> "resend" :> ReqBody '[JSON] Resend :> Post '[JSON] ResendResponse
      :<|> "partyinfo" :> ReqBody '[JSON] PartyInfo :> Post '[JSON] PartyInfo
      :<|> "upcheck" :> Get '[JSON] Text
--------------------------------------------------------------------------------
-- * Client
--------------------------------------------------------------------------------

postSendClient :: SendRequest -> ClientM SendResponse
postReceiveClient :: ReceiveRequest -> ClientM ReceiveResponse
postReceiveRawClient :: ReceiveRequest -> ClientM ReceiveResponse
postDeleteClient :: DeleteRequest -> ClientM NoContent

postSendClient
  :<|> postReceiveClient
  :<|> postReceiveRawClient
  :<|> postDeleteClient = client (Proxy @API)

pushClient :: EncryptedPayload -> ClientM Text
resendClient :: Resend -> ClientM ResendResponse
partyInfoClient :: PartyInfo -> ClientM PartyInfo
upcheckClient :: ClientM Text

pushClient
  :<|> resendClient
  :<|> partyInfoClient
  :<|> upcheckClient = client (Proxy @P2P)
--------------------------------------------------------------------------------
-- * Server
--------------------------------------------------------------------------------

newtype ProxyHandler a =
  ProxyHandler { runProxyHandler :: I.ReaderT ClientEnv (ExceptT ServantErr IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, I.MonadReader ClientEnv, MonadError ServantErr)

server :: ServerT API ProxyHandler
server = postSend
  :<|> postReceiveRaw
  :<|> postReceive
  :<|> postDelete

postSend :: SendRequest -> ProxyHandler SendResponse
postSend sReq = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (postSendClient sReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postSendRaw :: ClientEnv -> Application
postSendRaw constellationEnv =
  let (ClientEnv mgr (BaseUrl _ hst prt _)) = constellationEnv
      dest = ProxyDest (C8.pack hst) prt
  in waiProxyTo (const . return $ WPRProxyDest dest) defaultOnExc mgr

postReceive :: ReceiveRequest -> ProxyHandler ReceiveResponse
postReceive rReq = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (postReceiveClient rReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postReceiveRaw :: ReceiveRequest -> ProxyHandler ReceiveResponse
postReceiveRaw rReq = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (postReceiveRawClient rReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postDelete :: DeleteRequest -> ProxyHandler NoContent
postDelete dReq = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (postDeleteClient dReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp


p2pServer :: ServerT P2P ProxyHandler
p2pServer = push
  :<|> resend
  :<|> partyInfo
  :<|> upcheck

push :: EncryptedPayload -> ProxyHandler Text
push epl = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (pushClient epl) constellationEnv
  either (throwError . clientErrToServantErr) return resp

resend :: Resend -> ProxyHandler ResendResponse
resend rsend = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (resendClient rsend) constellationEnv
  either (throwError . clientErrToServantErr) return resp

partyInfo :: PartyInfo -> ProxyHandler PartyInfo
partyInfo pinfo = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM (partyInfoClient pinfo) constellationEnv
  either (throwError . clientErrToServantErr) return resp

upcheck :: ProxyHandler Text
upcheck = do
  constellationEnv <- I.ask
  resp <- liftIO $ runClientM upcheckClient constellationEnv
  either (throwError . clientErrToServantErr) return resp


--------------------------------------------------------------------------------
-- ** Application
--------------------------------------------------------------------------------

type NodeApi = (API :<|> P2P) :<|> RawSend

app :: ClientEnv -> Application
app env =
  logStdoutDev $ serve (Proxy @NodeApi) $ enter (phi env) (server :<|> p2pServer) :<|> postSendRaw env

initProxyApp :: IO ()
initProxyApp = do
  mprt <- lookupEnv "PROXY_PORT"
  env <- mkConstellationEnv
  case read <$> mprt of
    Nothing   -> error "Missing Env Var: PROXY_PORT"
    Just port -> run port $ app env

initProxyApp' :: ClientEnv -> Int -> IO ()
initProxyApp' env port = run port $ app env

phi :: ClientEnv -> ProxyHandler :~> Handler
phi env = NT $ \m -> runProxyHandler m & flip I.runReaderT env & Handler

mkConstellationEnv :: IO ClientEnv
mkConstellationEnv = do
  mgr <- newManager tlsManagerSettings
  ebaseUrl <- runExceptT makeBaseUrl
  case ebaseUrl of
    Left e        -> error e
    Right baseUrl -> return $ ClientEnv mgr baseUrl

makeBaseUrl :: ExceptT String IO BaseUrl
makeBaseUrl = do
    sch <- lookupEnv "CONSTELLATION_SCHEME" !? "Missing Env Var: CONSTELLATION_SCHEME" >>= parseScheme
    host <- lookupEnv "CONSTELLATION_HOST" !? "Missing Env Var: CONSTELLATION_HOST"
    port <- fmap read (lookupEnv "CONSTELLATION_PORT" !? "Missing Env Var: CONSTELLATION_PORT")
    path <- lookupEnv "CONSTELLATION_PATH" !? "Missing Env Var: CONSTELLATION_PATH"
    return $ BaseUrl sch host port path
  where
    parseScheme :: MonadError String m => String -> m Scheme
    parseScheme sch = case sch of
      "Http"  -> return Http
      "Https" -> return Https
      _       -> throwError "URI scheme must be Http or Https."

--------------------------------------------------------------------------------
clientErrToServantErr :: ServantError -> ServantErr
clientErrToServantErr sErr = case sErr of
  FailureResponse{..} -> ServantErr (H.statusCode responseStatus) "" responseBody []
  DecodeFailure{..} -> ServantErr 422 "" responseBody []
  UnsupportedContentType{..} -> ServantErr 422 "" responseBody []
  InvalidContentTypeHeader{..} -> ServantErr 415 "" responseBody []
  ConnectionError{..} -> err500 {errBody = "Connection Error" }

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
