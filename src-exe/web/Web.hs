{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad.Except
import           Happstack.Server.FileServe
import           Happstack.Server.SimpleHTTP
import           Rest.Run                    (apiToHandler)
import           System.Directory

import           Api                         (api)
import           Api.Types

main :: IO ()
main = do
  authExists <- doesFileExist "auth"
  unless authExists $ do
    putStrLn "Writing default login to ./auth: user=trustee pass=1234"
    writeFile "auth" "trustee/1234"

  jsConfigExists <- doesFileExist "ui/config.js"
  unless jsConfigExists $ do
    putStrLn "Writing default JS configuration to ./ui/config.js"
    writeFile "ui/config.js" "var appConfig = { apiHost : '' };\n"

  putStrLn "Starting server on http://localhost:3000"
  let serverData = ServerData
  simpleHTTP nullConf { port = 3000 } $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp

router :: Root Response
router = msum
  [ dir "api" $ do
      setHeaderM "Content-Disposition" "*"
      toResponse <$> apiToHandler api
  , serveDirectory DisableBrowsing ["index.html"] "ui"
  , dir "package" serveIndex
  , dir "latest" serveIndex
  ]
  where
    serveIndex = serveFile (asContentType "text/html") "ui/index.html"
