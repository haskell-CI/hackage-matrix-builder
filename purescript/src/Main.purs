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
import Uri (Uri, newUri, windowUri)
import Uri as Uri

type MainEffs e h o = Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) o

main :: forall e h. MainEffs (err :: EXCEPTION | e) h Unit
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

renderPackages :: forall e h . STRef h State -> MainEffs e h Unit
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
         Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
      -> Char
      -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) JQuery
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
    clickChar ::
         Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
      -> Char
      -> JQueryEvent
      -> JQuery
      -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
    clickChar showPrefix_ char _ev _el = do
      modifySTRef state \st -> st { selectedPrefix = char }
      showPrefix_

-- TODO: Shouldn't need to mention the api effect
click' :: forall e
   . (JQueryEvent -> JQuery -> Eff (api :: API, console :: CONSOLE, dom :: DOM | e) Unit)
  -> JQuery
  -> Eff (api :: API, console :: CONSOLE, dom :: DOM | e) Unit
click' f = on "click" (\ev el -> stopPropagation ev *> preventDefault ev *> f ev el)

tagsContent :: forall e h . STRef h State -> JQuery -> JQuery -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) (Array JQuery)
tagsContent state onlyReports pkgList = do
  st <- readSTRef state
  traverse renderedTag <<< Array.filter (\t -> Array.length t.packages > 0) $ st.allTags
  where
    renderedTag :: Tag -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) JQuery
    renderedTag t = do
      el <- renderTag t.name
      click' click el
      pure el
    click :: JQueryEvent -> JQuery -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
    click ev el = do
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
 -> Maybe { ghcVersion :: VersionName
          , packageVersion :: VersionName
          }
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

cellHash :: { packageName    :: PackageName
            , ghcVersion     :: VersionName
            , packageVersion :: VersionName
            } -> String
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
    onErr :: forall e1 h1 . Error -> MainEffs e1 h1 Unit
    onErr e = unsafeLog { a : "onErr", error: e }
    onOk :: forall e2 h2
       . { package :: Either Error Package
         , report  :: Either Error ShallowReport
         }
      -> MainEffs e2 h2 Unit
    onOk pr = renderPackage api pkgName $ either (const Nothing) (\p' -> Just { package : p', report : eToMay pr.report }) pr.package

eToMay :: forall e a . Either e a -> Maybe a
eToMay = either (const Nothing) Just

renderPackage :: forall e h
   . MatrixApi
  -> PackageName
  -> Maybe { package :: Package, report :: Maybe ShallowReport }
  -> MainEffs e h Unit
renderPackage api pkgName pr = do
  hidePages
  J.setText pkgName =<< J.select "#page-package .main-header"
  J.setHtml "" =<< J.select "#package"

  case pr of
    Just { package : pkg, report : Just report } -> do
      renderTable pkgName pkg
      let s = "Last build: " <> Misc.formatDate report.modified
      J.setText s =<< J.select("#page-package .main-header-subtext.last-build")
      renderSingleVersionMatrix api pkgName pkg report
      J.display =<< J.select ".package-header"
      J.display =<< J.select ".logs-header"
      J.hide =<< J.select "#package-not-built"
    Just { package : pkg, report : Nothing } -> do
      J.hide =<< J.select ".package-header"
      J.hide =<< J.select ".logs-header"
      renderTable pkgName pkg
      J.display =<< J.select "#package-not-built"
    Nothing -> unsafeThrow "renderPackage didn't get a Package"
  setupBuildQueuer api pkgName
--  setupTagger pkgName
  cleanupTabs
  J.display =<< J.select "#buildreport"
  J.display =<< J.select "#page-package"

setupBuildQueuer api pkgName = do
  log $ "setupBuildQueuer for " <> pkgName
  cleanupBuildQueuer
  void <<< runAff onErr onOk $ Api.queueByName api pkgName
  click' click =<< J.select "#queueing .action"
  where
    click :: forall h e . JQueryEvent -> JQuery -> Eff (api :: API, console :: CONSOLE, dom :: DOM, st :: ST h | e) Unit
    click ev el = do
      prio' <- Misc.val =<< J.select "#queueing .prio"
      log prio'
      let prio = case prio' of
                   "low" -> Low
                   "medium" -> Medium
                   "high" -> High
                   s -> unsafeThrow $ "Invalid value in queue priority select box: " <> s
      unsafeLog prio
      void <<< runAff onErr' onOk' $ Api.queueCreate api pkgName prio
      where
        onOk' :: forall e2 h2 . Unit -> MainEffs e2 h2 Unit
        onOk' _ = do
          log "Queued package"
        onErr' e = unsafeLog
          { a     : "Creating queue item failed"
          , error : e
          }
    onOk :: forall e2 h2
       . Maybe QueueItem
      -> MainEffs e2 h2 Unit
    onOk mqi = do
      case mqi of
        Nothing ->
          log "Queued"
        Just qi -> do
          unsafeLog { queueItem : qi }
          J.display =<< J.select "#queueing .already-queued"
    onErr e = unsafeLog
      { a     : "Loading queue data for package failed"
      , error : e
      }

cleanupBuildQueuer = do
  J.display =<< J.select "#queueing .form"
  -- TODO $("#queueing .action").off("click");
  J.hide =<< J.select "#queueing .success"
  J.hide =<< J.select "#queueing .error"
  J.hide =<< J.select "#queueing .already-queued"

setupTagger _pkgName = unsafeThrow "setupTagger"

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

renderSingleVersionMatrix :: forall e h
   . MatrixApi
  -> PackageName
  -> Package
  -> ShallowReport
  -> MainEffs e h Unit
renderSingleVersionMatrix api pkgName pkg report = do
  when (Array.null report.results) $ warn "empty reports array"
  flip traverseWithIndex report.results \i ghcResult -> do
    g :: ShallowGhcResult <- pure ghcResult
    let ghcVersionName = ghcResult.ghcVersion :: String
    let ghcFullVersionName = ghcResult.ghcFullVersion
    flip traverseWithIndex ghcResult.ghcResult \j versionResult -> do
      _ :: ShallowVersionResult <- pure versionResult
      versionName :: VersionName <- pure versionResult.packageVersion
      revision :: Revision <- pure versionResult.packageRevision
      res :: ShallowResult <- pure versionResult.result

      let ss = "#package .pkgv .revision[data-version='" <> versionName <> "']"
      th <- J.select $ ss
      mth <- selectFirst $ ss
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

      J.removeClass "fail-unknown" td
      let f cls txt = do
            J.addClass cls td
            J.setText txt td
      let onlyHighlight = highlightCell ghcVersionName versionName
      let onlyHighlightClick = click' (\_ _ -> onlyHighlight) td
      case res of
        ShallowOk -> do
          f "pass-build" "OK"
          onlyHighlightClick
        ShallowNop -> do
          f "pass-no-op" "OK (boot)"
          onlyHighlightClick
        ShallowNoIp -> do
          f "pass-no-ip" "OK (no-ip)"
          onlyHighlightClick
        ShallowNoIpBjLimit w -> do
          f "fail-bj" ("FAIL (BJ " <> show w <> ")")
          onlyHighlightClick
        ShallowNoIpFail -> do
          f "fail-no-ip" "FAIL (no-ip)"
          noIpFailClick pkgName td
        ShallowFail -> do
          f "fail-build" "FAIL (pkg)"
          failClick api pkgName td
        ShallowFailDeps w -> do
          f "fail-dep-build" ("FAIL (" <> show w <> " deps)")
          failDepsClick api pkgName td
      pure unit
    pure unit
  pure unit

-- TODO untested
noIpFailClick :: forall e h . PackageName -> JQuery -> MainEffs e h Unit
noIpFailClick pkgName td = do
  click' f td
  where
    f ev _el = do
      el <- Misc.selectElement =<< Misc.target ev
      ghcVersion <- Misc.getAttr "data-ghc-version" el
      packageVersion <- Misc.getAttr "data-package-version" el
      let p = { packageName : pkgName
              , ghcVersion : ghcVersion
              , packageVersion : packageVersion
              }
      setHash $ cellHash p
      setupFailTabs p ""
      {- (r.err <> "\n" <> r.out) -}

failClick :: forall e h . MatrixApi -> PackageName -> JQuery -> MainEffs e h Unit
failClick api pkgName = click' f
  where
    f ev _el = do
      el <- Misc.selectElement =<< Misc.target ev
      ghcVersion <- Misc.getAttr "data-ghc-version" el
      packageVersion <- Misc.getAttr "data-package-version" el
      let p = { packageName : pkgName
              , ghcVersion : ghcVersion
              , packageVersion : packageVersion
              }
      let ident = ghcVersion <> "-" <> packageVersion
      setHash $ cellHash p
      void <<< runAff onErr (onOk p) $ do
        res <- Api.singleResult api pkgName ident
        pure res
    onOk :: forall e2 h2
       . { ghcVersion :: VersionName
         , packageName :: PackageName
         , packageVersion :: VersionName
         }
      -> SingleResult
      -> MainEffs e2 h2 Unit
    onOk p sr = do
      case sr.resultA of
        Nothing -> unsafeLog "resultA was Nothing"
        Just vr ->
          case vr.result of
            Fail err -> setupFailTabs p err
            r -> unsafeLog
              { a : "Unexpected result, wanted Fail but got: "
              , result: vr
              }
      pure unit
    onErr e = unsafeLog
      { a     : "Loading cell data failed"
      , error : e
      }

failDepsClick :: forall e h . MatrixApi -> PackageName -> JQuery -> MainEffs e h Unit
failDepsClick api pkgName = click' f
  where
    f ev _el = do
      el <- Misc.selectElement =<< Misc.target ev
      ghcVersion <- Misc.getAttr "data-ghc-version" el
      packageVersion <- Misc.getAttr "data-package-version" el
      let p = { packageName : pkgName
              , ghcVersion : ghcVersion
              , packageVersion : packageVersion
              }
      setHash $ cellHash p
      let ident = ghcVersion <> "-" <> packageVersion
      void <<< runAff onErr (onOk p) $
        Api.singleResult api pkgName ident
    onOk :: forall e2 h2
       . { ghcVersion :: VersionName
         , packageName :: PackageName
         , packageVersion :: VersionName
         }
      -> SingleResult
      -> MainEffs e2 h2 Unit
    onOk p sr = do
      case sr.resultA of
        Nothing -> unsafeLog "resultA was Nothing"
        Just vr ->
          case vr.result of
            FailDeps dfs -> do
              unsafeLog "Successfully fetched FailDeps SingleResult"
              setupFailDepsTabs p dfs
            r -> unsafeLog
              { a : "Unexpected result, wanted Fail but got: "
              , result: vr
              }
      pure unit
    onErr e = unsafeLog
      { a     : "Loading cell data failed"
      , error : e
      }

setHash :: forall e . String -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
setHash hash = do
  uri <- Uri.newUri <$> windowUri
  unless (Uri.anchor uri == Just hash) do
    Misc.historyReplaceState "" $ Uri.withAnchor hash uri

setupFailTabs :: forall e h
   . { ghcVersion :: VersionName
     , packageName :: PackageName
     , packageVersion :: VersionName
     }
  -> String
  -> MainEffs e h Unit
setupFailTabs p r = do
  unsafeLog "setupFailTabs"
  pre <- J.create "<pre>"
  J.addClass "log-entry" pre
  J.setText r pre
  let label = cellHash p
  setupTabs [{ label : label, contents : pre }]
  highlightCell p.ghcVersion p.packageVersion

setupFailDepsTabs :: forall e h
   . { ghcVersion :: VersionName
     , packageName :: PackageName
     , packageVersion :: VersionName
     }
  -> Array DepFailure
  -> MainEffs e h Unit
setupFailDepsTabs p dfs = do
  xs <- flip traverseWithIndex dfs $ \i df -> do
    contents <- do
      div <- do
        div <- J.create "<div>"
        let pkgLink = packageLink df.packageName $ Just { ghcVersion : p.ghcVersion, packageVersion : df.packageVersion }
        -- J.setText pkgLink div
        pure div
      pre <- do
        pre <- J.create "<pre>"
        J.addClass "log-entry" pre
        J.setText df.message pre
        pure pre
      J.append pre div
      pure div
    let hash = cellHash { ghcVersion     : p.ghcVersion
                        , packageName    : df.packageName
                        , packageVersion : df.packageVersion
                        }
    pure { label    : hash
         , contents : contents
         }
  setupTabs xs
  highlightCell p.ghcVersion p.packageVersion

setupTabs :: forall e h . Array { label :: String, contents :: JQuery } -> MainEffs e h Unit
setupTabs messages = do
  unsafeLog "setupTabs"
  cleanupTabs
  tabs <- do
    tabs <- J.create "<div>"
    J.setAttr "id" "tabs" tabs
    pure tabs
  ul <- J.create "<ul>"
  headers <- flip traverseWithIndex messages \i v -> do
    a <- do
      a <- J.create "<a>"
      let fragment = "#fragment-" <> show (i + 1)
      J.setAttr "href" fragment a
      J.setText v.label a
      pure a
    li <- J.create "<li>"
    J.append a li
    pure li
  content <- flip traverseWithIndex messages \i v -> do
    div <- J.create "<div>"
    let fragment = "fragment-" <> show (i + 1)
    J.setAttr "id" fragment div
    J.append v.contents div
    pure div
  traverse_ (\h -> J.append h ul) headers
  J.append ul tabs
  traverse (\c -> J.append c tabs) content
  Misc.tabs tabs
  tabsContainer <- J.select "#tabs-container"
  J.append tabs tabsContainer
  -- $(window).scrollTo("#tabs-container");

cleanupTabs :: forall e h . MainEffs e h Unit
cleanupTabs = J.setHtml "" =<< J.select "#tabs-container"

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
