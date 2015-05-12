{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module WebServer (defaultMain) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Utils
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.String.ToString
import           Data.Time
import           Database.Persist.Sqlite      (SqlBackend, runMigration,
                                               runSqlite)
import           Happstack.Server.Compression
import           Happstack.Server.FileServe
import           Happstack.Server.SimpleHTTP
import           Rest.Run                     (apiToHandler)
import           System.Directory
import           System.FilePath

import           Api                          (api)
import qualified Api.Package                  as P
import           Api.Root
import           Api.Types                    (PackageName (..))
import           Config
import qualified Db.Queue                     as Q
import qualified Types.Queue                  as Q

defaultMain :: IO ()
defaultMain = do
  cfg <- defaultConfig
  let serverData = ServerData { config = cfg }

  assertFile (cs $ authFile cfg) . cs $ authUser cfg <> "/" <> authPass cfg
  assertFile (cs $ uiConfigFile cfg) "var appConfig = { apiHost : '' };\n"
  assertFile (cs $ packagesJson cfg) "[]"

  pns <- doesFileExist "packageNames.json"
  unless pns $ do
    putStrLn $ "Writing defaults to packageNames.json"
    L.writeFile (cs $ packageNamesJson cfg) . encode . sort . fromMaybe [] =<< P.loadPackageSummary

  putStrLn "Migrating sqlite database"
  runSqlite (sqliteDb cfg) $ runMigration Q.migrateQueue
  runSqlite (sqliteDb cfg) tryMigrateQueue

  putStrLn "Starting server on port 3000"

  let conf = nullConf { port = webServerPort cfg }
  s <- bindIPv4 (cs $ webServerHostName cfg) (port conf)
  simpleHTTPWithSocket s conf $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp
  waitForTermination

tryMigrateQueue :: MonadIO m => ReaderT SqlBackend m ()
tryMigrateQueue = do
  ex <- liftIO $ doesDirectoryExist "queue"
  when ex $ do
    liftIO $ putStrLn "Migrating queue to sqlite"
    items <- liftIO $ list
    forM_ items $ \(pkg,prio,modi) -> do
      liftIO . putStrLn $ "Migrating queue item: " ++ show (pkg,prio,modi)
      Q.add pkg prio (Just modi)
      liftIO $ removeFile (pth pkg)
    liftIO $ putStrLn "Deleting queue directory"
    liftIO $ removeDirectory "queue"
  where
    pth :: PackageName -> FilePath
    pth = ("queue" </>) . toString
    list :: IO [(PackageName,Q.Priority,UTCTime)]
    list
      =  fmap catMaybes
      .  mapM (get . PackageName . cs)
     =<< getDirWithFilter (not . ("." `isPrefixOf`)) "queue"
    get :: PackageName -> IO (Maybe (PackageName,Q.Priority,UTCTime))
    get p = do
      let fp = pth p
      de <- doesFileExist fp
      if de
        then do
          ts   <- getModificationTime fp
          prio <- fromMaybe Q.Medium . decodeV . cs <$> S.readFile fp
          return $ Just (p,prio,ts)
        else return Nothing
    getDirWithFilter :: MonadIO m => (FilePath -> Bool) -> FilePath -> m [String]
    getDirWithFilter p = liftIO . fmap (filter p) . getDirectoryContents

assertFile :: FilePath -> String -> IO ()
assertFile fp contents = do
  ex <- doesFileExist fp
  unless ex $ do
    putStrLn $ "Writing defaults to " ++ fp
    writeFile fp contents

router :: Root Response
router = void compressedResponseFilter >> msum
  [ dir "api" $ do
      setHeaderM "Content-Disposition" "*"
      toResponse <$> apiToHandler api
  , serveDirectory DisableBrowsing ["index.html"] "ui"
  , dir "package" serveIndex
  , dir "latest" serveIndex
  , dir "packages" serveIndex
  ]
  where
    serveIndex = serveFile (asContentType "text/html") "ui/index.html"
