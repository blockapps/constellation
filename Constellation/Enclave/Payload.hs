{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Constellation.Enclave.Payload where

import           ClassyPrelude

import           Control.Error
import           Control.Monad.Trans           (lift)
import qualified Crypto.Saltine.Class          as S
import qualified Crypto.Saltine.Core.Box       as Box
import qualified Crypto.Saltine.Core.SecretBox as SBox
import           Data.Aeson
import           Data.Binary                   (Binary (get, put))
import           Data.ByteArray.Encoding       (Base (Base64), convertFromBase,
                                                convertToBase)
import           Data.Maybe                    (fromJust)
import qualified Data.Text.Encoding            as T

data EncryptedPayload = EncryptedPayload
    { eplSender    :: Box.PublicKey
    , eplCt        :: ByteString
    , eplNonce     :: SBox.Nonce
    , eplRcptBoxes :: [ByteString]
    , eplRcptNonce :: Box.Nonce
    } deriving Eq

bytestringText :: ByteString -> Text
bytestringText = T.decodeUtf8 . convertToBase Base64


textBytestring :: Text -> Either String ByteString
textBytestring = convertFromBase Base64 . T.encodeUtf8

instance Show EncryptedPayload where
    show = show . encodeable

instance ToJSON EncryptedPayload where
  toJSON epl =
    object [ "sender" .= (bytestringText . S.encode . eplSender $ epl)
           , "ct" .= (bytestringText . eplCt $ epl)
           , "nonce" .= (bytestringText . S.encode . eplNonce $ epl)
           , "recipientBoxes" .= (bytestringText <$> eplRcptBoxes epl)
           , "recipientNonce" .= (bytestringText . S.encode . eplRcptNonce $ epl)
           ]

instance FromJSON EncryptedPayload where
    parseJSON (Object v) = do
      eres <- runExceptT $ do
        sndr <- do
          bsender <- ExceptT $ textBytestring <$> v .: "sender"
          maybe (fail "Could not parse sender.") return $ S.decode bsender
        ct <- ExceptT $ textBytestring <$> v .: "ct"
        nonce <- do
          bsnonce <- ExceptT $ textBytestring <$> v .: "nonce"
          maybe (fail "Could not parse Nonce.") return $ S.decode bsnonce
        rboxes <- do
          (txtBoxes :: [Text]) <- lift $ v .: "recipientBoxes"
          traverse (hoistEither . textBytestring) txtBoxes
        rnonce <- do
          bsnonce <- ExceptT $ textBytestring <$> v .: "recipientNonce"
          maybe (fail "Could not parse RecipientNonce.") return $ S.decode bsnonce
        return $ EncryptedPayload sndr ct nonce rboxes rnonce
      case eres of
        Left e    -> fail e
        Right epl -> return epl
    parseJSON _ = fail "Could not parse EncryptedPayload."

encodeable :: EncryptedPayload
           -> (ByteString, ByteString, ByteString, [ByteString], ByteString)
encodeable EncryptedPayload{..} =
    ( S.encode eplSender
    , eplCt
    , S.encode eplNonce
    , eplRcptBoxes
    , S.encode eplRcptNonce
    )

instance Binary EncryptedPayload where
    put = put . encodeable
    get = get >>= \(sender, ct, nonce, rcptBoxes, rcptNonce) -> return EncryptedPayload
        { eplSender    = fromJust $ S.decode sender
        , eplCt        = ct
        , eplNonce     = fromJust $ S.decode nonce
        , eplRcptBoxes = rcptBoxes
        , eplRcptNonce = fromJust $ S.decode rcptNonce
        }

encrypt :: ByteString
        -> Box.PublicKey
        -> Box.SecretKey
        -> [Box.PublicKey]
        -> IO EncryptedPayload
encrypt pl sender pk rcpts = encrypt' pl sender cks
  where
    cks = map (safeBeforeNM sender pk) rcpts

safeBeforeNM :: Box.PublicKey -> Box.SecretKey -> Box.PublicKey -> Box.CombinedKey
safeBeforeNM sender pk rcpt
    | sender == rcpt = error "encrypt: Sender cannot be a recipient"
    | otherwise      = Box.beforeNM pk rcpt

encrypt' :: ByteString
         -> Box.PublicKey
         -> [Box.CombinedKey]
         -> IO EncryptedPayload
encrypt' pl eplSender cks = do
    (mk, eplNonce, eplCt) <- sboxSeal pl
    eplRcptNonce          <- Box.newNonce
    let eplRcptBoxes = map (\ck -> Box.boxAfterNM ck eplRcptNonce emk) cks
        emk          = S.encode mk
    return EncryptedPayload{..}

sboxSeal :: ByteString -> IO (SBox.Key, SBox.Nonce, ByteString)
sboxSeal pt = do
    nonce <- SBox.newNonce
    mk    <- SBox.newKey
    let ct = SBox.secretbox mk nonce pt
    return (mk, nonce, ct)

decrypt :: ByteString
        -> SBox.Nonce
        -> ByteString
        -> Box.Nonce
        -> Box.PublicKey
        -> Box.SecretKey
        -> Either String ByteString
decrypt ct nonce rcptBox rcptNonce senderPub pk =
    decrypt' ct nonce rcptBox rcptNonce ck
  where
    ck = Box.beforeNM pk senderPub

decrypt' :: ByteString
         -> SBox.Nonce
         -> ByteString
         -> Box.Nonce
         -> Box.CombinedKey
         -> Either String ByteString
decrypt' ct nonce rcptBox rcptNonce ck = do
    emk <- Box.boxOpenAfterNM ck rcptNonce rcptBox `note'` "Couldn't open box after nm."
    mk <- S.decode emk `note'` "Couldn't decode box contents."
    SBox.secretboxOpen mk nonce ct `note'` "Couldn't open secret box"
  where
    note' = flip note
