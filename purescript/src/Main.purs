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
import Data.Foreign.Null
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
import Uri (Uri, newUri)
import Uri as Uri
import Types
import MiscFFI as Misc

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
  tl   <- Api.tagList      api
  pl   <- Api.packageList  api { count : Just 100000, offset : Nothing }
  adam <- Api.userByName   api "AdamBergmark"
  let state = { allTags         : tl.items
              , allPackages     : map (\p -> p.name) pl.items
              , allPackagesMore : pl.items
              }
  liftEff $ do
    log "Got everything from the API"
  setupRouting
  setupPicker

setupRouting :: forall e . Aff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
setupRouting = do
  liftEff $ log "setupRouting"
  liftEff $ Misc.onPopstate $ \pev -> log "onPopState"
  bd :: JQuery <- liftEff body
  liftEff $ Misc.delegate2 bd "a" "click" $ \e -> do
    log "clicked an anchor"
    thisAnchor <- Misc.selectElement =<< Misc.target e
    alt   <- Misc.altKey   e
    ctrl  <- Misc.ctrlKey  e
    meta  <- Misc.metaKey  e
    shift <- Misc.shiftKey e
    which <- Misc.which    e
    if alt || ctrl || meta || shift || which /= 1
      then do
        log "special url"
        Misc.unsafeLog
          { alt   : alt
          , ctrl  : ctrl
          , meta  : meta
          , shift : shift
          , which : which
          }
        pure unit
      else do
        log "not special url"
        preventDefault e
        stopPropagation e
        Misc.delay $ do
          linkUri <- newUri <$> Misc.getAttr "href" thisAnchor
          fromUri linkUri false false

fromUri :: forall e . Uri -> Boolean -> Boolean -> Eff (dom :: DOM | e) Unit
fromUri uri force isPopping = do
  currentUri <- newUri <$> Uri.windowUri
--  unless (not force && Uri.path uri == path currentUri) $ do
  r <- if unNull (Uri.path uri) == Just "/"
    then do
      renderHome
      pure { title : Nothing, pkgName : Nothing }
    else do
      case getVersionedPackageName uri of
        Just tmp -> do
          let pkgName = tmp.packageName
          let title = Just $ pkgName <> " - package"
          -- TODO was: packageUri(pkgName, tmp.packageVersion);
          let uri = packageUri pkgName Nothing
          selectedPackage pkgName
          pure { title : Just title, pkgName : Just pkgName }
        Nothing -> unsafeThrow "TODO"
  pure unit

renderHome :: forall e . Eff (dom :: DOM | e) Unit
renderHome = pure unit

setupPicker :: forall e . Aff (dom :: DOM, console :: CONSOLE, api :: API | e) Unit
setupPicker = pure unit

showTag :: Tag -> String
showTag t = "{ name : " <> show t.name <> ", packages : " <> show t.packages <> "}"

ghcVersions :: Array String
ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10", "8.0"]

type State =
  { allTags         :: Array Tag
  , allPackages     :: Array PackageName
  , allPackagesMore :: Array PackageMeta
  }

packageUri :: PackageName -> Maybe { ghcVersion :: VersionName, packageVersion :: VersionName } -> Uri
packageUri pkgName ghcAndPkgVersion =
  let u = newUri $ "/package/" <> pkgName in
  case ghcAndPkgVersion of
    Nothing -> u
    Just r -> flip Uri.withAnchor u $
      cellHash
        { packageName    : pkgName
        , ghcVersion     : r.ghcVersion
        , packageVersion : r.packageVersion
        }

cellHash :: { packageName :: PackageName, ghcVersion :: VersionName, packageVersion :: VersionName } -> String
cellHash r = "GHC-" <> r.ghcVersion <> "/" <> r.packageName <> "-" <> r.packageVersion

selectedPackage :: forall e . PackageName -> Eff e Unit
selectedPackage pkgName = unsafeThrow "selectedPackage"
