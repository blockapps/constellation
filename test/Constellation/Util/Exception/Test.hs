{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Constellation.Util.Exception.Test where

import           ClassyPrelude                hiding (assert)
import qualified Control.Exception            as E
import           Test.QuickCheck.Monadic      (PropertyM, assert, monadicIO,
                                               run)
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.QuickCheck        (testProperty)

import           Constellation.Util.Exception (someExceptionToStringEither,
                                               trys)

data MockException = MockException String
                   deriving Show

instance Exception MockException

tests :: TestTree
tests = testGroup "Util.Exception"
    [ testTrys
    , testSomeExceptionToStringEither
    ]

testTrys :: TestTree
testTrys = testProperty "trys" $ \s -> monadicIO $ do
    ee <- run $ trys (throw $ MockException s)
    trysAssert ee s

trysAssert :: Monad m => Either String () -> String -> PropertyM m ()
trysAssert ee s = assert $ ee == Left ("MockException " ++ show s)

testSomeExceptionToStringEither :: TestTree
testSomeExceptionToStringEither = testProperty "someExceptionToStringEither" $ \s -> monadicIO $ do
    ee <- run $ E.try (throw $ MockException s)
    trysAssert (someExceptionToStringEither ee) s
