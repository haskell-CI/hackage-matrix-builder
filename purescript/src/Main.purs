module Main where

-- import Control.Monad.Trans ()
-- import Data.Date (Date)
-- import Data.Map (Map)
-- import Data.Maybe (Maybe)
-- import Data.StrMap (StrMap)
-- import Data.Tuple (Tuple)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Eff.JQuery (ready)
import DOM (DOM)
import Data.List (List)
import Data.Set (Set)
import Prelude

import MatrixApi as Api
import MatrixApi
import Types

main :: forall e. Eff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
main = do
  api <- newApi "/api" "/api"
  log "main"
  ready do
    log "Ready"
    Api.tagList api
      (\taglist -> logShow <<< map showTag $ taglist.items)
      (const $ log "Failed to get tags")
  pure unit

showTag :: Tag -> String
showTag t = "{ name : " <> show t.name <> ", packages : " <> show t.packages <> "}"

ghcVersions :: Array String
ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10", "8.0"]

type State = { allTags :: Array Tag }
--  { allPackages     :: Array String
--  , allPackagesMore :: StrMap { name   :: String
--                              , report :: Report
--                              , tags   :: Tags
--                              }
--  }
