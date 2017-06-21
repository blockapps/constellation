{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Constellation.Configure.Main where

import           ClassyPrelude
import           System.Console.Haskeline (InputT, defaultSettings, runInputT)

defaultMain :: IO ()
defaultMain = runInputT defaultSettings configure

configure :: MonadIO m => InputT m ()
configure = do
    putStrLn "hi"
