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
import Data.Function.Uncurried
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
  tl <- Api.tagListAff api
  pl <- Api.packageListAff api { count : Just 100000, offset : Nothing }
  adam <- Api.userByNameAff api "AdamBergmark"
  let state = { allTags         : tl.items
              , allPackages     : map (\p -> p.name) pl.items
              , allPackagesMore : pl.items
              }
  liftEff $ do
    logShow $ map showTag state.allTags
    logShow $ adam.packages

showTag :: Tag -> String
showTag t = "{ name : " <> show t.name <> ", packages : " <> show t.packages <> "}"

ghcVersions :: Array String
ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10", "8.0"]

type State =
  { allTags         :: Array Tag
  , allPackages     :: Array PackageName
  , allPackagesMore :: Array PackageMeta
  }
