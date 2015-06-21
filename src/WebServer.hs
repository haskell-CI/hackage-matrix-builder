{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module WebServer (defaultMain) where

import           Control.Monad.Except
import           Data.String.Conversions
import           Happstack.Server.Compression
import           Happstack.Server.FileServe
import           Happstack.Server.SimpleHTTP
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
  assertFile (packagesJson cfg) "[]"
  assertDir  (queueDir cfg)

  putStrLn "Starting server on port 3000"

  let conf = nullConf { port = webServerPort cfg }
  s <- bindIPv4 (cs $ webServerHostName cfg) (port conf)
  simpleHTTPWithSocket s conf $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp
  waitForTermination

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
