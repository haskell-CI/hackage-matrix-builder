module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (ready)
import Data.Set (Set)
import DOM (DOM)
import Control.Monad.Trans ()
import Data.List (List)
import Data.Map (Map)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Date (Date)

import MatrixApi as Api
import MatrixApi (API, newApi, MatrixApi)
import Types

main :: forall e. Eff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
main = do
  api <- newApi "/api" "/api"
  log "main"
  ready do
    log "Ready"
    main' api
    -- userByName api "AdamBergmark" (\n -> log ("name: " ++ n.name)) (\err -> log "error")
  pure unit

main' :: forall e. MatrixApi -> Eff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
main' api = do
  Api.tagList api (\tags -> cont { allTags = tags }) (\_ -> pure unit)
  pure unit

cont st = pure unit

ghcVersions :: Array String
ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10", "8.0"]

type State = { allTags :: List Tag }
--  { allPackages     :: List String
--  , allPackagesMore :: StrMap { name   :: String
--                              , report :: Report
--                              , tags   :: Tags
--                              }
--  }
