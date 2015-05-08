{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad.Trans     (liftIO)
import           Data.String.ToString
import           Database.Persist        (Entity (Entity))
import           Database.Persist.Sqlite (runSqlite)
import qualified Queue

main :: IO ()
main = do
    let sqliteCfg = "db.sqlite"
    runSqlite sqliteCfg $ do
      q <- Queue.list
      liftIO $ case q of
          [] -> return ()
          (Entity _ Queue.Queue{..}:_) -> putStrLn (toString queuePackageName)
