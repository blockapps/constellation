{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Constellation.TestUtil where

import           ClassyPrelude

import           Constellation.Node.Api                as NodeApi
import qualified Constellation.Node.ServantApi         as SApi
import           Control.Concurrent                    (forkIO)
import qualified Data.HashMap.Strict                   as HM
import           Network.HTTP.Client                   (newManager)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import qualified Network.Wai.Handler.Warp              as Warp
import           Servant.Client
import           Test.Tasty                            (TestTree, testGroup)
import           Test.Tasty.HUnit                      (Assertion, testCase,
                                                        (@?=))

import           Constellation.Enclave                 (enclaveDecryptPayload,
                                                        enclaveEncryptPayload,
                                                        newEnclave')
import           Constellation.Enclave.Key             (newKeyPair)
import           Constellation.Enclave.Types           (PublicKey)
import           Constellation.Node                    (addParty, newNode,
                                                        receivePayload,
                                                        sendPayload)
import           Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import           Constellation.Node.Types              (Crypt (Crypt, decryptPayload, encryptPayload),
                                                        Node (Node, nodePi),
                                                        PartyInfo (piRcpts, piUrl))
import           Constellation.Util.Network            (getUnusedPort)
import           Constellation.Util.Text               (tformat)

setupTestNode :: FilePath -> String -> IO (ThreadId, ThreadId, TVar Node)
setupTestNode d name = do
    -- setup proxy node
    pport    <- getUnusedPort
    nport    <- getUnusedPort
    mgr <- newManager tlsManagerSettings
    let burl = BaseUrl Http "localhost" nport ""
        env = ClientEnv mgr burl
    pid <- forkIO $ SApi.initProxyApp' env pport
    -- setup node
    kp1@(pub1, _) <- newKeyPair
    kp2@(pub2, _) <- newKeyPair
    e             <- newEnclave' [kp1, kp2]
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    storage <- berkeleyDbStorage $ d </> name
    nvar    <- newTVarIO =<<
        newNode crypt storage (tformat "http://localhost:{}/" [pport])
        [pub1, pub2] [] []
    nid <- forkIO $ Warp.run nport $ NodeApi.app Nothing Private nvar
    return (nid, pid, nvar)

link :: TVar Node -> TVar Node -> STM ()
link nvar onvar = readTVar onvar >>= \onode ->
    addParty (piUrl $ nodePi onode) nvar

testSendPayload :: TVar Node -> TVar Node -> Assertion
testSendPayload nvar onvar = do
    [node, onode] <- atomically $ mapM readTVar [nvar, onvar]
    let pl   = "foo bar baz"
        from = firstPublicKey node
        to   = firstPublicKey onode
    es <- sendPayload node pl from [to]
    let key = case partitionEithers es of
                ([], [k]) -> k
                _ -> error "testSendPayload: Invalid response from sendPayload"
    -- Verify that the recipient node can retrieve and decrypt the payload
    epl <- receivePayload onode key to
    case epl of
        Left err  -> error $ "testSendPayload: Recipient: Error receiving payload: " ++ err
        Right pl' -> pl' @?= pl
    -- Verify that the sender node can retrieve and decrypt the payload
    epl' <- receivePayload node key to
    case epl' of
        Left err  -> error $ "testSendPayload: Sender: Error receiving payload: " ++ err
        Right pl' -> pl' @?= pl

firstPublicKey :: Node -> PublicKey
firstPublicKey Node{..} = case ownPubs of
    ((pub, _):_) -> pub
    _            -> error "firstPublicKey: Not found"
  where
    ownPubs = filter (\(_, url) -> url == piUrl nodePi)
        (HM.toList $ piRcpts nodePi)

kvTest :: (Show a, Eq b, Show b) => String -> [(a, b)] -> (a -> b) -> TestTree
kvTest name tests f = testGroup name $ map gen tests
  where
    gen (input, output) = testCase (show input) $ f input @?= output
