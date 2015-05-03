module WebServer (defaultMain) where

import           Control.Monad.Except
import           Happstack.Server.Compression
import           Happstack.Server.FileServe
import           Happstack.Server.SimpleHTTP
import           Rest.Run                     (apiToHandler)
import           System.Directory

import           Api                          (api)
import           Api.Root

defaultMain :: IO ()
defaultMain = do
  assertFile "auth" "trustee/1234"
  assertFile "ui/config.js" "var appConfig = { apiHost : '' };\n"
  assertFile "packages.json" "[]"
  createDirectoryIfMissing False "queue"

  putStrLn "Starting server on port 3000"
  let serverData = ServerData


  let conf = nullConf { port = 3000 }
  s <- bindIPv4 "127.0.0.1" (port conf)
  simpleHTTPWithSocket s conf $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp
  waitForTermination

assertFile :: FilePath -> String -> IO ()
assertFile fp contents = do
  ex <- doesFileExist "ui/config.js"
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
