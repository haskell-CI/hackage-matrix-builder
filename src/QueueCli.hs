{-# LANGUAGE RecordWildCards #-}
module QueueCli (defaultMain) where

import           Data.String.Conversions

import qualified Queue

defaultMain :: IO ()
defaultMain = do
    q <- Queue.list
    case q of
        [] -> return ()
        (Queue.QueueItem {..}:_) -> putStrLn (cs qPackageName)
