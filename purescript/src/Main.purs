module Main (main) where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Unsafe.Coerce
import Control.Monad.Eff.Console (CONSOLE, log, logShow, warn)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Control.Monad.Eff.JQuery
import Control.Monad.Eff.JQuery as J
import Control.Monad.ST
import DOM
import DOM.HTML.Types
import Data.Array ((!!))
import Data.Array as Array
import Data.Char
import Data.Date
import Data.Either
import Data.Foldable
import Data.Foldable as Foldable
import Data.Foreign.Undefined
import Data.Function.Uncurried
import Data.List ((..))
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.StrMap as StrMap
import Data.String as String
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.Traversable
import Data.Tuple
import Global as Global
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
  setupRouting api state
  setupPicker api state

setupRouting :: forall e h . MatrixApi -> STRef h State -> Aff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
setupRouting api state = do
  liftEff $ log "setupRouting"
  liftEff $ Misc.onPopstate $ \pev -> log "onPopState"
  bd :: JQuery <- liftEff body
  liftEff $ do
    currentUri <- Uri.newUri <$> Uri.windowUri
    fromUri api state currentUri true true
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
            fromUri api state linkUri false false

fromUri :: forall e h . MatrixApi -> STRef h State -> Uri -> Boolean -> Boolean -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
fromUri api state uri_ force isPopping = do
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
            selectedPackage api state pkgName
            pure { title : Just title, packageName : Just pkgName }
          Nothing ->
            case getPackageName uri_ of
              Just pkgName -> do
                let title = pkgName <> " - package"
                let uriForPackage = packageUri pkgName Nothing
                selectedPackage api state pkgName
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
  pure unit

setupPicker :: forall e h . MatrixApi -> STRef h State -> Aff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
setupPicker api state = liftEff $ do
  st <- readSTRef state
  select "#search" >>= Misc.autocomplete
    { source : st.allPackages
    , select : \v -> fromUri api state (packageUri v.item.value Nothing) false false
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
ghcVersions = ["8.0", "7.10", "7.8", "7.6", "7.4", "7.2", "7.0"]

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

selectedPackage :: forall e h . MatrixApi -> STRef h State -> PackageName -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
selectedPackage api state pkgName = do
  st <- readSTRef state
  log $ "selectedPackage: " <> show pkgName
  pure unit
  if pkgName `notElem` st.allPackages
    then renderNotFound (Just pkgName)
    else do
      log $ "found package: " <> pkgName
      void $ runAff onErr onOk $ do
        p :: Either Error Package       <- attempt $ Api.packageByName api pkgName
        r :: Either Error ShallowReport <- attempt $ Api.latestReportByPackageName api pkgName
        pure { package : p, report : r }
  where
    onErr :: forall e1 . Error -> Eff (console :: CONSOLE | e1) Unit
    onErr e = unsafeLog { a : "onErr", error: e }
    onOk :: forall e2
       . { package :: Either Error Package
         , report  :: Either Error ShallowReport
         }
      -> Eff (console :: CONSOLE, dom :: DOM | e2) Unit
    onOk pr = renderPackage pkgName $ either (const Nothing) (\p' -> Just { package : p', report : eToMay pr.report }) pr.package

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

eToMay :: forall e a . Either e a -> Maybe a
eToMay = either (const Nothing) Just

renderPackage :: forall e
   . PackageName
  -> Maybe { package :: Package, report :: Maybe ShallowReport }
  -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderPackage pkgName pr = do
  hidePages
  J.setText pkgName =<< J.select "#page-package .main-header"
  J.setHtml "" =<< J.select "#package"

  case pr of
    Just { package : pkg, report : Just report } -> do
      renderTable pkgName pkg
      let s = "Last build: " <> Misc.formatDate report.modified
      J.setText s =<< J.select("#page-package .main-header-subtext.last-build")
      renderSingleVersionMatrix pkgName pkg report
      J.display =<< J.select ".package-header"
      J.display =<< J.select ".logs-header"
      J.hide =<< J.select "#package-not-built"
    Just { package : pkg, report : Nothing } -> do
      J.hide =<< J.select ".package-header"
      J.hide =<< J.select ".logs-header"
      renderTable pkgName pkg
      J.display =<< J.select "#package-not-built"
    Nothing -> unsafeThrow "renderPackage didn't get a Package"
--  setupBuildQueuer pkgName
--  setupTagger pkgName
--  cleanupTabs
  J.display =<< J.select "#buildreport"
  J.display =<< J.select "#page-package"


setupBuildQueuer _pkgName = unsafeThrow "setupBuildQueuer"

setupTagger _pkgName = unsafeThrow "setupTagger"
-- cleanupTabs :: forall e . Eff (console :: CONSOLE, dom :: DOM | e) Unit
-- cleanupTabs = unsafeThrow "cleanupTabs"

renderTable :: forall e .PackageName -> Package -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderTable pkgName pkg = do
  let cols = Array.length ghcVersions
  let rows = Array.length pkg.versions
  t <- J.create "<table>"
  corner <- do
    th <- J.create "<th>"
    a <- J.create "<a>"
    J.setAttr "href" ("https://hackage.haskell.org/package/" <> pkgName) a
    J.setText pkgName a
    J.append a th
    pure th
  (\x -> unsafeLog { corner : x }) =<< J.getHtml corner
  headers <- flip traverse ghcVersions \ghcVersion -> do
    th <- J.create "<th>"
    J.setText ghcVersion th
    pure th

  thead <- J.create "<thead>"
  firstRow <- do
    tr <- J.create "<tr>"
    J.append corner tr
    traverse (\th -> J.append th tr) headers
    pure tr
  J.append firstRow thead
  J.append thead t

  -- TODO Do this last
  ppackage <- J.select "#package"
  J.append t ppackage

  flip traverse_ (List.reverse (0..(rows-1))) \row -> do
    let mv = pkg.versions !! row
    case mv of
      Nothing -> unsafeLog { msg : "Couldn't find version for index", index : show row }
      Just v -> do
        let versionName = v.version
        let revision = v.revision
        let unpreferred = v.preference == UnPreferred
        let previousVersionNameMay = (\p -> p.version) <$> (pkg.versions !! (row-1))
        unsafeLog { v : v
                  , versionName            : versionName
                  , revision               : revision
                  , preference             : v.preference
                  , unpreferred            : unpreferred
                  , previousVersionNameMay : previousVersionNameMay
                  }
        th <- do
          th <- J.create "<th>"
          J.addClass "pkgv" th
          aDiff <- do
            a <- J.create "<a>"
            J.setText "Δ" a
            J.setAttr "href" (Uri.toString $ hdiffUrl pkgName versionName previousVersionNameMay) a
            pure a
          aHackageUrl <- do
            a <- J.create "<a>"
            J.setAttr "href" (hackageUrl pkgName versionName) a
            J.setText versionName a
            pure a

          supRevision <- case revision of
            0 -> pure Nothing
            rev -> do
              sup <- J.create "<sup>"
              a <- J.create "<a>"
              J.setText ("(" <> show rev <> ")") a
              J.setAttr "href" (revisionsUrl pkgName versionName) a
              J.setAttr "data-revision" (show rev) a
              J.setAttr "data-version" versionName a
              J.addClass "revision" a
              J.append a sup
              pure $ Just sup
          J.append aDiff th
          J.appendText " " th
          J.append aHackageUrl th
          traverse (\s -> J.append s th) supRevision
          pure th

        tds <- flip traverse ghcVersions \ghcVersionName -> do
          td <- J.create "<td>"
          J.addClass "stcell" td
          J.addClass "fail-unknown" td
          J.setAttr "data-ghc-version" ghcVersionName td
          J.setAttr "data-package-version" versionName td
          pure td

        tr <- do
          tr <- J.create "<tr>"
          J.addClass "solver-row" tr
          J.append th tr
          traverse_ (\x -> J.append x tr) tds
          pure tr

        let prev = (\x -> splitVersion x.version) <$> (pkg.versions !! (row + 1))
        let curr = splitVersion v.version

        when (newMajor prev curr) do
          J.addClass "first-major" tr
        when (newMinor prev curr) do
          J.addClass "first-minor" tr

        J.append tr t

splitVersion :: VersionName -> Array String
splitVersion v = String.split (String.Pattern ".") v

newMajor :: Maybe (Array String) -> Array String -> Boolean
newMajor ma b = case ma of
  Nothing -> true
  Just a -> (a !! 0) /= (b !! 0)
         || (fromMaybe "0" (a !! 1) /= fromMaybe "0" (b !! 1))

newMinor :: Maybe (Array String) -> Array String -> Boolean
newMinor ma b = case ma of
  Nothing -> true
  Just a -> newMajor ma b
         || ((fromMaybe "0" (a !! 2)) /= (fromMaybe "0" (b !! 2)))

hdiffUrl :: PackageName -> VersionName -> Maybe VersionName -> Uri
hdiffUrl pkgName versionName prevVersionName =
  Uri.newUri $ case prevVersionName of
    Just prev -> pref <> "/diff?id=" <> versionName <> "&id2=" <> prev
    Nothing -> pref <> "/commit?id=" <> versionName
  where
    pref = "http://hdiff.luite.com/cgit/" <> pkgName

hackageUrl :: PackageName -> VersionName -> Uri
hackageUrl pkgName versionName =
  Uri.newUri $"https://hackage.haskell.org/package/" <> pkgName <> "-" <> versionName <> "/" <> pkgName <> ".cabal/edit"

revisionsUrl :: PackageName -> VersionName -> Uri
revisionsUrl pkgName versionName =
  Uri.newUri $ "https://hackage.haskell.org/package/" <> pkgName <> "-" <> versionName <> "/revisions"

renderSingleVersionMatrix :: forall e
   . PackageName
  -> Package
  -> ShallowReport
  -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
renderSingleVersionMatrix pkgName pkg report = do
  when (Array.null report.results) $ warn "empty reports array"
  flip traverseWithIndex report.results \i ghcResult -> do
    g :: ShallowGhcResult <- pure ghcResult
    unsafeLog (Tuple i ghcResult)
    let ghcVersionName = ghcResult.ghcVersion :: String
    let ghcFullVersionName = ghcResult.ghcFullVersion
    flip traverseWithIndex ghcResult.ghcResult \j versionResult -> do
      _ :: ShallowVersionResult <- pure versionResult
      versionName :: VersionName <- pure versionResult.packageVersion
      revision :: Revision <- pure versionResult.packageRevision
      res :: ShallowResult <- pure versionResult.result -- TODO Fix ADT deserialization (?)

      th <- J.select $ "#package .pkgv .revision[data-version='" <> versionName <> "']"
      newestRevision <- Global.readInt 10 <$> Misc.getAttr "data-revision" th

      unless (unsafeCoerce revision == newestRevision) $
        J.addClass "newer-revision" th

      mtd <- selectFirst $ "#package td[data-ghc-version='" <> ghcVersionName <> "'][data-package-version='" <> versionName <> "']"
      td <- case mtd of
        Nothing -> unsafeThrow $ "Could not find cell for "
                <> (cellHash { packageName : pkgName
                             , ghcVersion : ghcVersionName
                             , packageVersion : "versionName"
                             })
        Just td -> pure td

      let onlyHighlight = highlightCell ghcVersionName versionName
      J.removeClass "fail-unknown" td
      -- unsafeLog (Tuple "res" res (res == )
      pure unit
    pure unit
  pure unit

highlightCell :: forall e . VersionName -> VersionName -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
highlightCell ghcVersion packageVersion = do
  J.removeClass "highlight" =<< J.select "#page-package .stcell.highlight"
  tableCell <- selectFirst $ "#page-package .stcell[data-package-version='" <> packageVersion <> "'][data-ghc-version='" <> ghcVersion <> "']"
  case tableCell of
    Nothing -> warn $ "Could not find table cell for highlighting, ghcVersion: " <> ghcVersion <> ", packageVersion: " <> packageVersion
    Just c -> J.addClass "highlight" c
  pure unit

traverseWithIndex :: forall a b m
   . (Applicative m)
  => (Int -> a -> m b)
  -> Array a
  -> m (Array b)
traverseWithIndex f = sequence <<< Array.mapWithIndex f

selectFirst :: forall e
   . String
  -> Eff (dom :: DOM | e) (Maybe JQuery)
selectFirst = map Array.head <<< J.toArray <=< J.select
