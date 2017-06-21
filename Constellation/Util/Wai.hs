{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Constellation.Util.Wai where

import           ClassyPrelude

import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types   (badRequest400, hContentType,
                                       internalServerError500, notFound404,
                                       ok200, unauthorized401)
import qualified Network.Wai          as Wai

ok :: BL.ByteString -> Wai.Response
ok = Wai.responseLBS ok200 [(hContentType, "application/json; charset=utf-8")]

ok' :: ByteString -> Wai.Response
ok' = ok . BL.fromStrict

badRequest :: Wai.Response
badRequest = Wai.responseLBS badRequest400 [] "Bad Request"

unauthorized :: Wai.Response
unauthorized = Wai.responseLBS unauthorized401 [] "Unauthorized"

notFound :: Wai.Response
notFound = Wai.responseLBS notFound404 [] "Not Found"

internalServerError :: Wai.Response
internalServerError = Wai.responseLBS internalServerError500 [] "Internal Server Error"
