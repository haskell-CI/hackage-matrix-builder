module Main (main) where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Control.Monad.Eff.JQuery
import Control.Monad.Trans
import DOM
import Data.Date
import Data.List
import Data.Map
import Data.Maybe
import Data.Set (Set)
import Data.StrMap
import Data.Tuple
import Prelude

import MatrixApi as Api
import MatrixApi
import Types

type AllEffs e o = Eff (dom :: DOM, console :: CONSOLE, api :: API, err :: EXCEPTION) o

main :: forall e. AllEffs e Unit
main = do
  api <- newApi "/api" "/api"
  log "main"
  ready do
    log "Ready"
    launchAff $ boot api

boot :: forall e
   . MatrixApi
  -> Aff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
boot api = do
  liftEff $ log "bootCont"
  tl <- tagListAff api
  pl <- packageListAff api (Just 100000) Nothing
  let state = { allTags         : tl.items
              , allPackages     : map (\p -> p.name) pl.items
              , allPackagesMore : pl.items
              }
  liftEff $ logShow $ map showTag state.allTags

showTag :: Tag -> String
showTag t = "{ name : " <> show t.name <> ", packages : " <> show t.packages <> "}"

ghcVersions :: Array String
ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10", "8.0"]

type State =
  { allTags         :: Array Tag
  , allPackages     :: Array PackageName
  , allPackagesMore :: Array PackageMeta
  }

packageListAff :: forall e
   . MatrixApi
  -> Maybe Int
  -> Maybe Int
  -> Aff (api :: API | e) (ApiList PackageMeta)
packageListAff api mcount moffset = makeAff \err succ ->
  Api.packageList api mcount moffset succ (err <<< const (error "Getting package list failed"))

tagListAff :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (ApiList Tag)
tagListAff api = makeAff \err succ -> Api.tagList api succ (err <<< const (error "Getting tag list failed"))
