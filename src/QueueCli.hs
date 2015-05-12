{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module QueueCli (defaultMain) where

import           Control.Monad.Trans     (liftIO)
import           Data.String.Conversions
import           Database.Persist        (Entity (Entity))
import           Database.Persist.Sqlite (runSqlite)

import           Config                  (defaultConfig, sqliteDb)
import qualified Queue

defaultMain :: IO ()
defaultMain = do
    cfg <- defaultConfig
    runSqlite (cs $ sqliteDb cfg) $ do
      q <- Queue.list
      liftIO $ case q of
          [] -> return ()
          (Entity _ Queue.Queue{..}:_) -> putStrLn (cs queuePackageName)
