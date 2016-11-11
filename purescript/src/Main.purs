module Main (main) where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Control.Monad.Eff.JQuery
import Control.Monad.Eff.JQuery as J
import Control.Monad.Trans
import DOM
import Data.Array as Array
import Data.Date
import Data.Either
import Data.Function.Uncurried
import Data.List
import Data.Map
import Data.Maybe
import Data.Set (Set)
import Data.StrMap
import Data.String.Regex as R
import Data.Tuple
import Partial.Unsafe (unsafePartial)
import Prelude

import MatrixApi
import MatrixApi as Api
import MiscFFI (unsafeLog)
import MiscFFI as Misc
import Types
import Uri (Uri, newUri)
import Uri as Uri

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
  tl   <- Api.tagList     api
  pl   <- Api.packageList api { count : Just 100000, offset : Nothing }
  adam <- Api.userByName  api "AdamBergmark"
  let state = { allTags         : tl.items
              , allPackages     : map (\p -> p.name) pl.items
              , allPackagesMore : pl.items
              }
  liftEff $ do
    log "Got everything from the API"
  setupRouting
  setupPicker state

setupRouting :: forall e . Aff (api :: API, console :: CONSOLE, dom :: DOM | e) Unit
setupRouting = do
  liftEff $ log "setupRouting"
  liftEff $ Misc.onPopstate $ \pev -> log "onPopState"
  bd :: JQuery <- liftEff body
  liftEff $ do
    currentUri <- Uri.newUri <$> Uri.windowUri
    fromUri currentUri true true
    Misc.delegate2 bd "a" "click" $ \e -> do
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
          unsafeLog
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
            log "delay"
            linkUri <- newUri <$> Misc.getAttr "href" thisAnchor
            fromUri linkUri false false

fromUri :: forall e . Uri -> Boolean -> Boolean -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
fromUri uri force isPopping = do
  log "fromUri"
  let justTitle t = { title : Just t, packageName : Nothing }
  currentUri <- newUri <$> Uri.windowUri
  unsafeLog $ Tuple (Uri.path uri) (Uri.path currentUri)
  unless (not force && Uri.path uri == Uri.path currentUri) $ do
    r :: { title :: Maybe String, packageName :: Maybe String } <-
      if Uri.path uri == Just "/"
      then do
        renderHome
        pure { title : Nothing, packageName : Nothing }
      else do
        case getVersionedPackageName uri of
          Just tmp -> do
            let pkgName = tmp.packageName
            let pgkVersion = tmp.packageVersion
            let title = pkgName <> " - package"
            -- TODO was: packageUri(pkgName, tmp.packageVersion);
            let uri = packageUri pkgName Nothing
            selectedPackage pkgName
            pure { title : Just title, packageName : Just pkgName }
          Nothing ->
            case getPackageName uri of
              Just pkgName -> do
                let title = pkgName <> " - package"
                let uri = packageUri pkgName Nothing
                selectedPackage pkgName
                pure { title : Just title, packageName : Just pkgName }
              Nothing ->
                if Uri.path uri == Just "/latest"
                then do
                  renderLatest
                  pure $ justTitle "latest"
                else do
                  case R.match (regex' "^/user/([^/]+)" R.noFlags) <$> Uri.path uri of
                    Just (Just arr) ->
                      case Array.head arr of
                        Just (Just name) -> do
                          renderUser name
                          pure <<< justTitle $ name <> "- users"
                        x -> throwLog "TODO5" x
                    _ ->
                      if Uri.path uri == Just "/packages"
                        then do
                          renderPackages
                          pure $ justTitle "packages"
                        else do
                          renderNotFound
                          pure $ justTitle "404'd!"
    let title = maybe "" (\v -> v <> " - ") r.title <> "Hackage Matrix Builder"
    unsafeLog $ Tuple "title" title
    let history = if isPopping
                    then Misc.historyReplaceState
                    else Misc.historyPushState
    history title uri
    Misc.setDocumentTitle title
    pure unit

-- | Unsafe version of `regex`.
regex' :: String -> R.RegexFlags -> R.Regex
regex' pattern flags = unsafePartial $ fromRight (R.regex pattern flags)

throwLog :: forall a b e . String -> a -> Eff (console :: CONSOLE | e) b
throwLog err d = do
  unsafeLog ({ errorType : err, data : d })
  unsafeThrow err

renderNotFound :: forall e . Eff (dom :: DOM | e) Unit
renderNotFound = pure unit -- $ unsafeThrow "renderNotFound"

renderHome :: forall e . Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderHome = do
  log "renderHome"
  hidePages
  J.select "#page-home" >>= J.display

hidePages :: forall e . Eff (console :: CONSOLE, dom :: DOM | e) Unit
hidePages = do
  J.select ".page" >>= J.hide

renderPackages :: forall e . Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderPackages = do
  log "renderPackages"
  pure unit -- $ unsafeThrow "renderPackages"

renderLatest :: forall e . Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderLatest = do
  log "renderLatest"
  pure unit -- $ unsafeThrow "renderLatest"

renderUser :: forall e . String -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderUser _ = do
  log "renderUser"
  pure unit -- $ unsafeThrow "renderUser"

setupPicker :: forall e . State -> Aff (api :: API, console :: CONSOLE, dom :: DOM | e) Unit
setupPicker st = liftEff $ do
  let items = st.allPackages
  select "#search" >>= Misc.autocomplete
    { source : st.allPackages
    , select : \v -> fromUri (packageUri v.item.value Nothing) false false
    }
-- TODO
-- $("#search").keydown(function (e) {
--   if (e.which === 13) {
--     e.preventDefault();
--     e.stopPropagation();
--     fromUri(packageUri($(this).val()));
--     return;
--   }
-- });


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
