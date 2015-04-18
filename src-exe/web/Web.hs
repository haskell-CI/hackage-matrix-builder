{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad.Except
import           Happstack.Server.FileServe
import           Happstack.Server.SimpleHTTP
import           Rest.Run                    (apiToHandler)

import           Api                         (api)
import           Api.Types

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:3000"
  let serverData = ServerData
  simpleHTTP nullConf { port = 3000 } $ do
    (rsp,_) <- runRoot serverData $ getFilter router
    return rsp

router :: Root Response
router = msum
  [ dir "api" $ toResponse <$> apiToHandler api
  , serveDirectory DisableBrowsing ["index.html"] "ui"
  , dir "package" $ serveFile (asContentType "text/html") "ui/index.html"
  ]
