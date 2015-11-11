{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module WebServer (defaultMain) where

import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Data.Conduit                 (($$+-))
import           Data.Conduit.Binary          (sinkFile)
import           Data.String.Conversions
import           Happstack.Server.Compression (compressedResponseFilter)
import           Happstack.Server.FileServe   (Browsing (..), asContentType,
                                               serveDirectory, serveFile)
import           Happstack.Server.SimpleHTTP  (Response, bindIPv4, dir,
                                               getFilter, nullConf, port,
                                               setHeaderM, simpleHTTPWithSocket,
                                               toResponse, waitForTermination)
import           Network.HTTP.Conduit         (http, newManager, parseUrl,
                                               requestHeaders, responseBody,
                                               tlsManagerSettings)
import           Path
import           Rest.Run                     (apiToHandler)

import           Api                          (api)
import           Api.Root
import           Config
import           Paths

defaultMain :: IO ()
defaultMain = do
  cfg <- defaultConfig
  let serverData = ServerData { config = cfg }

  assertFile (authFile cfg) . cs $ authUser cfg <> "/" <> authPass cfg
  assertFile (uiConfigFile cfg) "var appConfig = { apiHost : '' };\n"
  assertPackagesJson (packagesJson cfg)
  assertFile (tagsFile cfg) "{}"
  assertDir  (queueDir cfg)

  putStrLn "Starting server on port 3000"

  let conf = nullConf { port = webServerPort cfg }
  s <- bindIPv4 (cs $ webServerHostName cfg) (port conf)
  simpleHTTPWithSocket s conf $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp
  waitForTermination

assertPackagesJson :: Path Rel File -> IO ()
assertPackagesJson fp = do
  ex <- doesFileExistP fp
  unless ex $ do
    putStrLn $ "Downloading package metadata from hackage to " ++ cs fp
    manager <- liftIO $ newManager tlsManagerSettings
    req_ <- parseUrl "http://hackage.haskell.org/packages/"
    let req = req_ { requestHeaders = [("Accept", "application/json")] }
    runResourceT $ do
      res <- http req manager
      responseBody res $$+- sinkFile (cs fp)


assertFile :: Path Rel File -> String -> IO ()
assertFile fp contents = do
  ex <- doesFileExistP fp
  unless ex $ do
    putStrLn $ "Writing defaults to " ++ cs fp
    writeFileP fp contents

assertDir :: Path Rel Dir -> IO ()
assertDir d = do
  ex <- doesDirectoryExistP d
  unless ex $ do
    putStrLn $ "Creating queue directory: " ++ toFilePath d
    createDirectoryP d

router :: Root Response
router = void compressedResponseFilter >> msum
  [ dir "api" $ do
      setHeaderM "Content-Disposition" "*"
      toResponse <$> apiToHandler api
  , serveDirectory DisableBrowsing ["index.html"] "ui"
  , dir "package"  serveIndex
  , dir "latest"   serveIndex
  , dir "packages" serveIndex
  , dir "user"     serveIndex
  ]
  where
    serveIndex = serveFile (asContentType "text/html") "ui/index.html"
