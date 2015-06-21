{-# LANGUAGE RecordWildCards   #-}
module QueueCli (defaultMain) where

import           Control.Monad.Trans     (liftIO)
import           Data.String.Conversions
import           Database.Persist        (Entity (Entity))
import           Database.Persist.Sqlite (runSqlite)

import           Config                  (defaultConfig, sqliteDb)
import qualified Queue

main :: IO ()
main = do
    q <- Queue.list
    case q of
        [] -> return ()
        (Queue.QueueItem {..}:_) -> putStrLn (toString qPackageName)
