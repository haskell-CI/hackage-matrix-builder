module Main (main) where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Control.Monad.Eff.JQuery
import Control.Monad.Eff.JQuery as J
import Control.Monad.ST
import DOM
import DOM.HTML.Types
import Data.Array as Array
import Data.Char
import Data.Date
import Data.Either
import Data.Foldable
import Data.Foldable as Foldable
import Data.Foreign.Undefined
import Data.Function.Uncurried
import Data.List ((..))
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.StrMap
import Data.String as String
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.Traversable
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

type AllEffs e h o = Eff (api :: API, console :: CONSOLE, dom :: DOM, err :: EXCEPTION, st :: ST h) o

main :: forall e h. AllEffs e h Unit
main = do
  unsafeLog Nothing
  unsafeLog (Just 1)
  api <- newApi "/api" "/api"
  log "main"
  ready do
    log "Ready"
    launchAff $ boot api

boot :: forall e h
   . MatrixApi
  -> Aff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
boot api = do
  liftEff $ log "bootCont"
  tl   <- Api.tagList     api
  pl   <- Api.packageList api { count : Undefined (Just 100000), offset : Undefined Nothing }
  liftEff $ unsafeLog pl
  -- adam <- Api.userByName  api "AdamBergmark"
  state <- liftEff $ newSTRef
    ({ allTags          : tl.items
     , allPackages      : map (\p -> p.name) pl.items
     , allPackagesMore  : pl.items
     , activeTagFilters : mempty
     , selectedPrefix   : 'A'
     } :: State)
  liftEff $ do
    log "Got everything from the API"
  setupRouting state
  -- setupPicker state

-- type State =
--   { allTags          :: Array Tag
--   , allPackages      :: Array PackageName
--   , allPackagesMore  :: Array PackageMeta
--   , activeTagFilters :: Array TagName
--   , selectedPrefix   :: Char
--   }

setupRouting :: forall e h . STRef h State -> Aff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
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

fromUri :: forall e h . STRef h State -> Uri -> Boolean -> Boolean -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
fromUri state uri_ force isPopping = do
  log $ "fromUri"
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
                  renderLatest state
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
                          renderPackages state
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

renderLatest :: forall e h . STRef h State -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h| e) Unit
renderLatest state = do
  log "renderLatest"
  pure unit

renderPackages :: forall e h . STRef h State -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
renderPackages state = do
  log "renderPackages"
  hidePages
  st <- readSTRef state
  page <- J.select "#page-packages"
  tags <- J.find ".tag-filter" page
  setHtml "" tags

  headersEl <- J.find ".headers" page
  setHtml "" headersEl
  pkgList <- J.find ".packages" page
  onlyReports <- J.find ".packages-only-reports" page
  let headers = map fromCharCode (65..90)
  J.removeClass "active" tags
  setHtml "" tags
  let showPrefix_ = showPrefix state onlyReports pkgList

  tagsCont <- tagsContent state onlyReports pkgList
  traverse_ (\t -> J.append t tags *> pure unit) tagsCont
  traverse_ (\h ->
    do t <- charHeader showPrefix_ h
       J.append t headersEl) headers
  on "change" (\_ _ -> showPrefix_) onlyReports
  showPrefix_
  J.select "#page-packages" >>= J.display
  where
    charHeader ::
         Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
      -> Char
      -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) JQuery
    charHeader showPrefix_ v = do
      li :: JQuery <- J.create "<li>"
      a <- J.create "<a>"
      J.setAttr "data-prefix" v a
      J.addClass "header" a
      J.setAttr "href" "" a
      J.setText (String.singleton v) a
      click' (clickChar showPrefix_ v) a
      J.append a li
      pure li
    clickChar showPrefix_ char _ev _el = do
      modifySTRef state \st -> st { selectedPrefix = char }
      showPrefix_

click' :: forall e
   . (JQueryEvent -> JQuery -> Eff (console :: CONSOLE, dom :: DOM | e) Unit)
  -> JQuery
  -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
click' f = on "click" (\ev el -> stopPropagation ev *> preventDefault ev *> f ev el)

tagsContent :: forall e h . STRef h State -> JQuery -> JQuery -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) (Array JQuery)
tagsContent state onlyReports pkgList = do
  st <- readSTRef state
  traverse renderedTag <<< Array.filter (\t -> Array.length t.packages > 0) $ st.allTags
  where
    renderedTag :: Tag -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) JQuery
    renderedTag t = do
      el <- renderTag t.name
      click' click el
      pure el
    click :: JQueryEvent -> JQuery -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
    click ev el = do
      unsafeLog { ev : ev, el : el }
      tagEl :: JQuery <- Misc.selectElement =<< Misc.target ev
      tagName :: String <- Misc.getAttr "data-tag-name" tagEl
      st <- readSTRef state
      let tagIndex = Array.elemIndex tagName st.activeTagFilters
      case tagIndex of
        Nothing -> do
          writeSTRef state $ st { activeTagFilters = tagName Array.: st.activeTagFilters }
          J.addClass "active" tagEl
        Just ti -> do
          writeSTRef state $ st { activeTagFilters = Array.delete tagName st.activeTagFilters }
          J.removeClass "active" tagEl
      showPrefix state onlyReports pkgList

showPrefix :: forall e h . STRef h State -> JQuery -> JQuery -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
showPrefix state onlyReports pkgList = do
  log "showPrefix"
  showOnlyReports <- Misc.is ":checked" onlyReports
  st :: State <- readSTRef state
  filterByTags <- pure (Array.length st.activeTagFilters >= 1)
  setHtml "" pkgList
  pkgListContents <- mkPkgListContents filterByTags showOnlyReports
  traverse_ (\v -> J.append v pkgList) pkgListContents
  pure unit
  where
    mkPkgListContents :: Boolean -> Boolean -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) (Array JQuery)
    mkPkgListContents filterByTags showOnlyReports = do
      st <- readSTRef state
      map Array.catMaybes <<< traverse m2 <<< Array.filter (fv st filterByTags showOnlyReports) $ st.allPackagesMore
    m2 :: PackageMeta -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) (Maybe JQuery)
    m2 pm = do
      pure Nothing
      st <- readSTRef state
      li <- J.create "<li>"
      pl <- packageLink pm.name Nothing
      renderedTags :: Array JQuery <- traverse renderTag pm.tags
      msmall <- case pm.report of
        Nothing -> pure Nothing
        Just date -> do
          small <- J.create "<small>"
          J.setText (" - last built: " <> Misc.formatDate date) small
          pure (Just small)
      J.append pl li
      traverse_ (\t -> J.append t li) renderedTags
      traverse_ (\s -> J.append s li) msmall
      pure $ Just li
    fv :: State -> Boolean -> Boolean -> PackageMeta -> Boolean
    fv st filterByTags showOnlyReports pm =
      ( if filterByTags
          then Array.any (\t -> Array.elem t st.activeTagFilters) pm.tags
          else map (\x -> x.head) (String.uncons pm.name) == Just st.selectedPrefix
      ) && (not showOnlyReports || hasReportForPackage st pm.name)

hasReportForPackage :: State -> PackageName -> Boolean
hasReportForPackage st pn =
  case Array.find (\v -> v.name == pn) st.allPackagesMore of
    Nothing -> false
    Just pm -> isJust pm.report

fromJustNote :: forall a . String -> Maybe a -> a
fromJustNote msg = fromMaybe (unsafeThrow msg)

renderTag :: forall e . String -> Eff (console :: CONSOLE, dom :: DOM | e) JQuery
renderTag tagName = do
  log $ "renderTag: " <> tagName
  a <- J.create "<a>"
  J.addClass "tag-item" a
  J.setAttr "data-tag-name" tagName a
  J.setText tagName a
  pure a

renderUser :: forall e . String -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderUser _ = do
  log "renderUser"
  pure unit -- $ unsafeThrow "renderUser"

setupPicker :: forall e h . STRef h State -> Aff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
setupPicker state = liftEff $ do
  st <- readSTRef state
  select "#search" >>= Misc.autocomplete
    { source : st.allPackages
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
  { allTags          :: Array Tag
  , allPackages      :: Array PackageName
  , allPackagesMore  :: Array PackageMeta
  , activeTagFilters :: Array TagName
  , selectedPrefix   :: Char
  }

packageLink :: forall e
  . PackageName
 -> Maybe { ghcVersion :: VersionName, packageVersion :: VersionName }
 -> Eff (dom :: DOM | e) JQuery
packageLink pkgName versions = do
  a <- J.create "<a>"
  setAttr "href" (Uri.toString (packageUri pkgName versions)) a
  setText pkgName a
  pure a

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

selectedPackage :: forall e h . STRef h State -> PackageName -> Eff (console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
selectedPackage state pkgName = do
  st <- readSTRef state
  log $ "selectedPackage: " <> show pkgName
  pure unit
  if pkgName `notElem` st.allPackages
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

renderPackage :: forall e
   . PackageName
  -> Maybe Package
  -> Maybe Report
  -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderPackage pkgName mp mr = unsafeThrow "renderPackage" {-do
  hidePages
  J.setText pkgName =<< J.select "#page-package .main-header"
  J.setHtml "" =<< J.select "#package"

  case Tuple mp mr of
    Tuple (Just pkg) (Just report) -> do
      renderTable pkgName pkg
      let s = "Last build: " <> Misc.formatDate report.modified
      J.setText s =<< J.select("#page-package .main-header-subtext.last-build")
      renderSingleVersionMatrix pkgName pkg report
      J.display =<< J.select ".package-header"
      J.display =<< J.select ".logs-header"
      J.hide =<< J.select "#package-not-built"
    Tuple (Just pkg) Nothing -> do
      J.hide =<< J.select ".package-header"
      J.hide =<< J.select ".logs-header"
      renderTable pkgName pkg
      J.display =<< J.select "#package-not-built"
    Tuple _ _ -> unsafeThrow "renderPackage case"
  setupBuildQueuer pkgName
  setupTagger pkgName
  cleanupTabs
  J.display =<< J.select "#buildreport"
  J.display =<< J.select "#page-package"
-}
setupBuildQueuer = unsafeThrow "setupBuildQueuer"
renderTable = unsafeThrow "renderTable"
renderSingleVersionMatrix = unsafeThrow "renderSingleVersionMatrix"
setupTagger = unsafeThrow "setupTagger"
cleanupTabs = unsafeThrow "cleanupTabs"
