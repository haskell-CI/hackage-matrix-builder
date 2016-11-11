module Main (main) where

import Data.Foldable
import Data.Traversable
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Control.Monad.Eff.JQuery
import Control.Monad.Eff.JQuery as J
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
import Data.String.Regex.Flags as RF
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
  setupRouting state
  setupPicker state

setupRouting :: forall e . State -> Aff (api :: API, console :: CONSOLE, dom :: DOM | e) Unit
setupRouting state = do
  liftEff $ log "setupRouting"
  liftEff $ Misc.onPopstate $ \pev -> log "onPopState"
  bd :: JQuery <- liftEff body
  liftEff $ do
    currentUri <- Uri.newUri <$> Uri.windowUri
    fromUri state currentUri true true
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
            fromUri state linkUri false false

fromUri :: forall e . State -> Uri -> Boolean -> Boolean -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
fromUri state uri_ force isPopping = do
  log "fromUri"
  let justTitle t = { title : Just t, packageName : Nothing }
  currentUri <- newUri <$> Uri.windowUri
  unsafeLog $ Tuple (Uri.path uri_) (Uri.path currentUri)
  unless (not force && Uri.path uri_ == Uri.path currentUri) $ do
    r :: { title :: Maybe String, packageName :: Maybe String } <-
      if Uri.path uri_ == Just "/"
      then do
        renderHome
        pure { title : Nothing, packageName : Nothing }
      else do
        case getVersionedPackageName uri_ of
          Just tmp -> do
            let pkgName = tmp.packageName
            let pgkVersion = tmp.packageVersion
            let title = pkgName <> " - package"
            -- TODO was: packageUri(pkgName, tmp.packageVersion);
            let uriForPackage = packageUri pkgName Nothing
            selectedPackage state pkgName
            pure { title : Just title, packageName : Just pkgName }
          Nothing ->
            case getPackageName uri_ of
              Just pkgName -> do
                let title = pkgName <> " - package"
                let uriForPackage = packageUri pkgName Nothing
                selectedPackage state pkgName
                pure { title : Just title, packageName : Just pkgName }
              Nothing ->
                if Uri.path uri_ == Just "/latest"
                then do
                  renderLatest
                  pure $ justTitle "latest"
                else do
                  case R.match (regex' "^/user/([^/]+)" RF.noFlags) <$> Uri.path uri_ of
                    Just (Just arr) ->
                      case Array.head arr of
                        Just (Just name) -> do
                          renderUser name
                          pure <<< justTitle $ name <> "- users"
                        x -> throwLog "TODO5" x
                    _ ->
                      if Uri.path uri_ == Just "/packages"
                        then do
                          renderPackages
                          pure $ justTitle "packages"
                        else do
                          renderNotFound Nothing
                          pure $ justTitle "404'd!"
    let title = maybe "" (\v -> v <> " - ") r.title <> "Hackage Matrix Builder"
    unsafeLog $ Tuple "title" title
    let history = if isPopping
                    then Misc.historyReplaceState
                    else Misc.historyPushState
    history title uri_
    Misc.setDocumentTitle title
    pure unit

-- | Unsafe version of `regex`.
regex' :: String -> RF.RegexFlags -> R.Regex
regex' pattern flags = unsafePartial $ fromRight (R.regex pattern flags)

throwLog :: forall a b e . String -> a -> Eff (console :: CONSOLE | e) b
throwLog err d = do
  unsafeLog ({ errorType : err, data : d })
  unsafeThrow err

renderNotFound :: forall e . Maybe PackageName -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderNotFound mPkgName = do
  log $ "renderNotFound: " <> show mPkgName
  hidePages
  msg <- case mPkgName of
    Nothing -> do
      el <- J.create "<div>"
      appendText "Page not found, sorry!" el
      pure el
    Just pkgName -> do
      el <- J.create "<div>"
      J.appendText "The package " el
      strongName <- J.create "<strong>"
      J.appendText pkgName strongName
      J.append strongName el
      J.appendText " could not be found." el
      pure el
  msgContainer <- J.select "#page-notfound .main-header-subtext"
  J.setHtml "" msgContainer
  J.append msg msgContainer
  J.select "#page-notfound" >>= J.display

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
setupPicker state = liftEff $ do
  select "#search" >>= Misc.autocomplete
    { source : state.allPackages
    , select : \v -> fromUri state (packageUri v.item.value Nothing) false false
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

selectedPackage :: forall e . State -> PackageName -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
selectedPackage state pkgName = do
  log $ "selectedPackage: " <> show pkgName
  pure unit
  if pkgName `notElem` state.allPackages
    then renderNotFound (Just pkgName)
    else do
      log $ "found package: " <> pkgName
  -- unsafeThrow "selectedPackage"

-- function selectedPackage (pkgName) {
--   $("#select-package").val(pkgName);
--   if (window.allPackages.indexOf(pkgName) === -1) {
--     renderNotFound(pkgName);
--     return;
--   }
--   api.Package.byName(pkgName).get(function (pkg) {
--     api.Package.byName(pkgName).Report.latest().get(renderPackage.bind(null, pkgName, pkg), renderPackage.bind(null, pkgName, pkg, null));
--   }, renderPackage.bind(null, pkgName, null, null, null));
-- }
