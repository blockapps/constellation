{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Constellation.Util.Network where

import           ClassyPrelude
import           Network.Socket (Family (AF_INET), SockAddr (SockAddrInet),
                                 SocketType (Stream), aNY_PORT, bind, close,
                                 iNADDR_ANY, socket, socketPort)

getUnusedPort :: IO Int
getUnusedPort = do
    sock <- socket AF_INET Stream 0
    bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
    port <- socketPort sock
    close sock
    return $ fromIntegral port

