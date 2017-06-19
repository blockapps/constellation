{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Constellation.Node.ServantApi where

import Data.Aeson
    (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:), (.=), object)
import Data.ByteString
import Data.Text (Text)
import Data.ByteArray.Encoding (Base(Base64), convertToBase, convertFromBase)
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai

import Constellation.Enclave.Types (PublicKey)
import Constellation.Util.ByteString (mustB64TextDecodeBs, b64BsDecodeText)

import Servant

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

data DeleteResponse = DeleteResponse deriving (Show)

type API = "send" :> ReqBody '[JSON] SendRequest :> Post '[JSON] SendResponse
      :<|> "sendRaw" :> Raw
      :<|> "receive" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "receiveRaw" :> ReqBody '[JSON] ReceiveRequest :> Post '[JSON] ReceiveResponse
      :<|> "delete" :> ReqBody '[JSON] DeleteRequest :> Post '[JSON] DeleteResponse
