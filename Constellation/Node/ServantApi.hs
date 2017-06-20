{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Constellation.Node.ServantApi where


import Control.Monad.IO.Class
import Control.Error
import Data.Aeson
    (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:), (.=), object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Control.Monad.Error.Class
import Data.Text (Text)
import Data.Proxy
import Data.ByteArray.Encoding (Base(Base64), convertToBase, convertFromBase)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)


import Servant.Server.Internal.ServantErr
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Types as H

import Constellation.Enclave.Types (PublicKey)
import Constellation.Util.ByteString (mustB64TextDecodeBs, b64BsDecodeText)

import Servant
import Servant.Client

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import           Network.Wai.Handler.Warp (run)

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
  parseJSON _ = fail "Could not parse SendResponse"

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

--------------------------------------------------------------------------------
-- | API
--------------------------------------------------------------------------------

type API = "send" :> ReqBody '[JSON] SendRequest :> Post '[JSON] SendResponse
      :<|> "sendRaw" :> Raw
      :<|> "receive" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "receiveRaw" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "delete" :> ReqBody '[JSON] DeleteRequest :> Post '[JSON] NoContent

--------------------------------------------------------------------------------
-- * Client
--------------------------------------------------------------------------------

postSendClient :: SendRequest -> ClientM SendResponse
postReceiveClient :: ReceiveRequest -> ClientM ReceiveResponse
postReceiveRawClient :: ReceiveRequest -> ClientM ReceiveResponse
postDeleteClient :: DeleteRequest -> ClientM NoContent

postSendClient
  :<|> _
  :<|> postReceiveClient
  :<|> postReceiveRawClient
  :<|> postDeleteClient = client (Proxy @API)

--------------------------------------------------------------------------------
-- * Server
--------------------------------------------------------------------------------

server :: Server API
server = postSend
  :<|> postSendRaw
  :<|> postReceive
  :<|> postReceive
  :<|> postDelete


postSend :: SendRequest -> Handler SendResponse
postSend sReq = do
  resp <- liftIO $ runClientM (postSendClient sReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postSendRaw :: Application
postSendRaw =
  let (ClientEnv mgr (BaseUrl _ hst prt _)) = constellationEnv
      dest = ProxyDest (C8.pack hst) prt
  in waiProxyTo (const . return $ WPRProxyDest dest) defaultOnExc mgr

postReceive :: ReceiveRequest -> Handler ReceiveResponse
postReceive rReq = do
  resp <- liftIO $ runClientM (postReceiveClient rReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postReceiveRaw :: ReceiveRequest -> Handler ReceiveResponse
postReceiveRaw rReq = do
  resp <- liftIO $ runClientM (postReceiveRawClient rReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

postDelete :: DeleteRequest -> Handler NoContent
postDelete dReq = do
  resp <- liftIO $ runClientM (postDeleteClient dReq) constellationEnv
  either (throwError . clientErrToServantErr) return resp

--------------------------------------------------------------------------------
-- ** Application
--------------------------------------------------------------------------------

app :: Application
app = logStdoutDev $ serve (Proxy @API) server

constellationEnv :: ClientEnv
constellationEnv = unsafePerformIO $ do
  mgr <- newManager tlsManagerSettings
  ebaseUrl <- makeBaseUrl
  case ebaseUrl of
    Left e -> error e
    Right baseUrl -> return $ ClientEnv mgr baseUrl
{-# NOINLINE constellationEnv #-}

makeBaseUrl :: IO (Either String BaseUrl)
makeBaseUrl = runExceptT $ do
    sch <- lookupEnv "CONSTELLATION_SCHEME" !? "Missing Env Var: CONSTELLATION_SCHEME" >>= parseScheme
    host <- lookupEnv "CONSTELLATION_HOST" !? "Missing Env Var: CONSTELLATION_HOST"
    port <- lookupEnv "CONSTELLATION_PORT" !? "Missing Env Var: CONSTELLATION_PORT" >>= return . read
    path <- lookupEnv "CONSTELLATION_PATH" !? "Missing Env Var: CONSTELLATION_PATH"
    return $ BaseUrl sch host port path
  where
    parseScheme :: MonadError String m => String -> m Scheme
    parseScheme sch = case sch of
      "Http" -> return Http
      "Https" -> return Https
      _ -> throwError "URI scheme must be Http or Https."

initProxyApp :: IO ()
initProxyApp = do
  mprt <- lookupEnv "PROXY_PORT"
  maybe (error "Missing Env Var: PROXY_PORT") (flip run app . read) mprt

--------------------------------------------------------------------------------
clientErrToServantErr :: ServantError -> ServantErr
clientErrToServantErr sErr = case sErr of
  FailureResponse{..} -> ServantErr (H.statusCode responseStatus) "" responseBody []
  DecodeFailure{..} -> ServantErr 422 "" responseBody []
  UnsupportedContentType{..} -> ServantErr 422 "" responseBody []
  InvalidContentTypeHeader{..} -> ServantErr 415 "" responseBody []
  ConnectionError{..} -> err500 {errBody = "Connection Error" }
