{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module QueueCli (defaultMain) where

import           Control.Monad.Trans     (liftIO)
import           Data.String.ToString
import           Database.Persist        (Entity (Entity))
import           Database.Persist.Sqlite (runSqlite)

import Config (sqliteDb, defaultConfig)
import qualified Queue

defaultMain :: IO ()
defaultMain = do
    sqliteCfg <- sqliteDb <$> defaultConfig
    runSqlite sqliteCfg $ do
      q <- Queue.list
      liftIO $ case q of
          [] -> return ()
          (Entity _ Queue.Queue{..}:_) -> putStrLn (toString queuePackageName)
