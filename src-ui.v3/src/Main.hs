{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Main (main) where

import           Control.Monad             (sequence_)
import           Data.Aeson                (FromJSON)
import qualified Data.Aeson                as J
import qualified Data.Aeson.Types          as J
import           Data.Bool                 (not)
import qualified Data.Char                 as C
import qualified Data.Foldable             as F
import qualified Data.JSString             as JSS
import qualified Data.JSString.Text        as JSS
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import qualified Data.Maybe                as M
import           Data.Monoid               (Endo (Endo), appEndo)
import qualified Data.List.NonEmpty        as NE
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import qualified Data.Text                 as T
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)
import           Data.Time                 (getCurrentTime)
import           Data.Time.Clock.POSIX     (POSIXTime, posixSecondsToUTCTime,
                                            utcTimeToPOSIXSeconds)
import           Data.Time.Format          (defaultTimeLocale, formatTime)

import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Data.Version              as Ver
import           GHC.Generics              (Rep)
import qualified GHCJS.DOM.Types           as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM as DOM
import           Language.Javascript.JSaddle (jsNull)
import           Network.URI
--import           Reflex.Dom
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.Router (route)
import           Reflex.Dom.Location
-- import           Reflex.Dom.Routing.Nested
import           Control.Lens hiding (children, element)
import           Control.Monad.Fix
import           Reflex.Dom.Widget.Basic
import           Reflex.Time
import           Reflex.Class
import           Servant.API
import           Servant.Reflex
import qualified Text.Read                    as R

import           API
import           PkgId
import           Router


main :: IO ()
main = mainWidget bodyElement4

-- WARNING: the code that follows will make you cry;
--          a safety pig is provided below for your benefit.
--
--                           _
--   _._ _..._ .-',     _.._(`))
--  '-. `     '  /-._.-'    ',/
--     )         \            '.
--    / _    _    |             \
--   |  a    a    /              |
--   \   .-.                     ;
--    '-('' ).-'       ,'       ;
--       '-;           |      .'
--          \           \    /
--          | 7  .__  _.-\   \
--          | |  |  ``/  /`  /
--         /,_|  |   /,_/   /
--            /,_/      '`-'
--

{-
bodyElement2 :: forall t m . (SupportsServantReflex t m, MonadWidget t m) => m ()
bodyElement2 = runRouteWithPathInFragment $ do
  switchPromptly never <=< withRoute $ \route -> case fromMaybe "" route of
    ""         -> (["users"] <$) <$> button "Open users"
    "users"    -> text "Users" >> pure never
    "test"     -> text "Test" >> pure never
    "settings" -> text "Settings" >> pure never
    _          -> redirectLocally []
-}

utc2unix :: UTCTime -> Int
utc2unix x = ceiling (realToFrac (utcTimeToPOSIXSeconds x) :: Double)

bodyElement4 :: forall t m . (SupportsServantReflex t m, MonadFix m, MonadIO m, MonadHold t m, PostBuild t m, DomBuilder t m, Adjustable t m, DomBuilderSpace m ~ GhcjsDomSpace) => m ()
bodyElement4 = do
  _ <- runRouteViewT app

  --(result, changeStateE) <- runSetRouteT $ app RouteHome 
  pure ()

app :: forall t m. (SetRoute t FragRoute m, SupportsServantReflex t m, MonadFix m, MonadIO m, MonadHold t m, PostBuild t m, DomBuilder t m, Adjustable t m, DomBuilderSpace m ~ GhcjsDomSpace) 
    => FragRoute -- Dynamic t FragRoute
    -> m ()
app dynFrag = do
  -- top-level PB event
  evPB0 <- getPostBuild

  t0 <- liftIO getCurrentTime

  dynClock <- clockLossy 1 t0
  let dynUTCTime = _tickInfo_lastUTC <$> dynClock

  ticker2 <- tickLossy 2 t0
  let ticker4 = ffilter ((==0) . flip rem 2 . _tickInfo_n) ticker2
      ticker8 = ffilter ((==0) . flip rem 4 . _tickInfo_n) ticker2

  -- we can use this
  evIdxStLast <- getIdxStates (leftmost [ticker8 $> (), evPB0 $> ()])
  dynIdxStLast <- holdUniqDyn =<< holdDyn (PkgIdxTs 0) evIdxStLast

  evPackages0 <- getPackages (updated dynIdxStLast $> ())
  dynPackages0 <- holdDyn mempty evPackages0

  -- pseudo navbar
  el "nav" $ do
    text "[ "
    routeLink False "#/" (text "HOME")
    text " | "
    routeLink False "#/queue" (text "Build Queue")
    text " | "
    routeLink False "#/packages" (text "Packages")
    text " ]"
    text "    (current index-state: "
    dynText (pkgIdxTsToText <$> dynIdxStLast)
    text " ; package count: "
    display (V.length <$> dynPackages0)
    text ")"

  -- search box
  _ <- searchBoxWidget dynPackages0
  el "hr" blank

  _ <- case dynFrag of --dyn $ dynFrag >>= \case
    RouteHome -> do
      elAttr "div" (("id" =: "page-home") <> ("class" =: "page")) $ do
        divClass "leftcol" $ do
          elAttr "h2" ("class" =: "main-header") $ text "Welcome"
          el "h3" $ text "Documents"
          el "ul" $ do
            el "li" $ 
              elAttr "a" ("href" =: "https://github.com/haskell-infra/hackage-trustees/blob/master/policy.md") $ 
                text "Hackage trustee policy and procedures"
            el "li" $
              elAttr "a" ("href" =: "https://wiki.haskell.org/Taking_over_a_package") $
                text "Wiki: Taking over a package"
            el "li" $
              elAttr "a" ("href" =: "https://wiki.haskell.org/Hackage_trustees") $
                text "Wiki: Hackage Trustee"
          el "h3" $ text "Trustee Tools"
          el "ul" $ do
            el "li" $ 
              elAttr "a" ("href" =: "https://github.com/haskell-infra/hackage-trustees/issues") $ 
                text "Issue Tracker"
            el "li" $
              elAttr "a" ("href" =: "https://www.github.com/haskell-CI/hackage-matrix-builder") $
                text "hackage-matrix-builder source"
            el "li" $
              elAttr "a" ("href" =: "https://www.github.com/haskell-CI/hackage-cli") $
                text "hackage-cli"
            el "li" $
              elAttr "a" ("href" =: "https://github.com/hackage-trustees") $
                text "GitHub organization"
            el "li" $
              elAttr "a" ("href" =: "http://hackage.haskell.org/packages/recent/revisions.html") $
                text "Recent Revisions"                
          el "h3" $ text "References"
          el "ul" $ do
            el "li" $ 
              elAttr "a" ("href" =: "https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory") $ 
                text "GHC Boot Library Version History"
            el "li" $
              elAttr "a" ("href" =: "https://ghc.haskell.org/trac/ghc/wiki/LanguagePragmaHistory") $
                text "Language Pragma History"
            el "li" $
              elAttr "a" ("href" =: "https://github.com/haskell/cabal/blob/641e854ae663e2b34f34ecc11ba663ac3a9bdc19/Cabal/Distribution/PackageDescription/Check.hs#L911-L1091") $
                text "Required cabal-version"
            el "li" $
              elAttr "a" ("href" =: "https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md") $
                text "Cookbook for common build failures"
      pure ()

    RouteQueue -> do
      evPB <- getPostBuild

      let dynUnixTime = utc2unix <$> dynUTCTime

      evWorkers <- getWorkers (leftmost [ticker2 $> (), evPB])
      dynWorkers <- holdUniqDyn =<< holdDyn mempty evWorkers
      let dynWorkers2 = fmap mkWorkerStat dynWorkers

      el "h1" $ text "Workers"
      el "div" $ do
        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" $ text "age"
              el "th" $ text "phase"
              el "th" $ text "pkg-name"
              el "th" $ text "pkg-ver"
              el "th" $ text "compiler"
              el "th" $ text "index-state"

          _ <- el "tbody" $ do
            simpleList (V.toList <$> dynWorkers) $ \wr -> el "tr" $ do

              el "td" $ display ((-) <$> dynUnixTime <*> (wrModified <$> wr))
              el "td" $ dynText ((T.pack . drop 2 . show . wrState) <$> wr)
              -- maybes
              let pn = fromMaybe (PkgN "") . wrPkgname <$> wr -- FIXME
              el "td" $ pkgLink pn
              el "td" $ dynText (maybe "" verToText . wrPkgversion <$> wr)
              el "td" $ dynText (maybe "" compilerIdToText . wrHcversion <$> wr)
              el "td" $ dynText (maybe mempty pkgIdxTsToText . wrIdxState <$> wr)
          pure ()
        pure ()

      el "h1" $ text "Queue"
      el "div" $ do
        -- aButton <- el "div" $ button "Refresh Queue"
        evQRows <- getQueue (leftmost [ticker4 $> (), evPB])
        dynQRows <- holdUniqDyn =<< holdDyn mempty evQRows

        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" $ text "qprio"
              el "th" $ text "pkg-name"
              el "th" $ text "index-state"
              el "th" $ text "workers"
              el "th" $ text "mtime(queue-entry)"

          el "tbody" $ do
            _ <- simpleList (V.toList <$> dynQRows) $ \qr -> do
              let -- pn :: Dynamic t PkgN
                  pn = qrPkgname   <$> qr
                  -- wcnt :: Dynamic t Int
                  wcnt = dynWorkers2 <*> pn <*> (qrIdxstate <$> qr)

              elDynAttr "tr" ((\x -> if x==0 then mempty else ("style" =: "background-color: #ffffd0")) <$> wcnt) $ do
                el "td" $ display (qrPriority  <$> qr)
                el "td" $ pkgLink pn
                el "td" $ dynText ((pkgIdxTsToText . qrIdxstate) <$> qr)
                el "td" $ dynText ((\x -> if x == 0 then "" else tshow x) <$> wcnt)
                el "td" $ display (qrModified  <$> qr)
            pure ()

      el "h1" $ text "Recent Uploads"
      el "div" $ do
        let lb = (\(PkgIdxTs t) -> PkgIdxTs (t - (24*60*60))) <$> dynIdxStLast

        evHistRows <- getPackagesHistory (QParamSome <$> lb) (QParamSome <$> dynIdxStLast) (leftmost [updated dynIdxStLast $> (), evPB])
        dynHistRows <- holdDyn mempty evHistRows

        dynShowRevs <- el "div" $ do
          tmp <- _checkbox_value <$> checkbox True (CheckboxConfig never mempty)
          text "show revisions"
          pure tmp

        dynShowRels <- el "div" $ do
          tmp <- _checkbox_value <$> checkbox True (CheckboxConfig never mempty)
          text "show releases"
          pure tmp

        let attrHideRevs = bool ("hidden" =: "") mempty <$> dynShowRevs
            attrHideRels = bool ("hidden" =: "") mempty <$> dynShowRels

        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" $ text "index-state"
              el "th" $ text "pkg-name"
              el "th" $ text "pkg-ver"
              el "th" $ text "rev"
              el "th" $ text "uploader"

          el "tbody" $ do
            _ <- dyn $ do
              rows <- V.toList . V.reverse <$> dynHistRows
              pure $ do
                forM_ rows $ \(IdxHistoryEntry is pn pv rv unam) -> do
                  let wcnt = dynWorkers2 <*> pure pn <*> pure is
                      isRev = rv > 0
                      rowAttr1 = (\x -> if x==0 then mempty else ("style" =: "background-color: #ffffd0")) <$> wcnt
                      rowAttr2 | isRev = attrHideRevs
                                | otherwise = attrHideRels

                  elDynAttr "tr" (rowAttr1 <> rowAttr2) $ do
                      el "td" $ text (pkgIdxTsToText is)
                      el "td" $ pkgLink (pure pn)
                      el "td" $ text (verToText pv)
                      el "td" $ text (if rv > 0 then "-r"<>tshow rv else "")
                      el "td" $ elAttr "a" ("href" =: ("#/user/" <> unam)) (text unam)

            pure ()
        pure ()

    RoutePackages -> do
        el "h1" $ text "Packages"
        evPB <- getPostBuild
        evTags<- getTags (constDyn $ QParamSome False) evPB
        dynTags <- holdDyn mempty evTags
        evTagPkgs<- getTagsPkg (constDyn $ QParamSome True) evPB
        dynTagPkgs <- holdDyn Map.empty evTagPkgs
        let dynPkgTags = pkgTagList <$> dynTagPkgs
        packagesPageWidget dynPackages0 dynTags dynPkgTags

    RoutePackage (PkgN pkgUri) -> do
        let pn = PkgN $ T.takeWhile (/='@') pkgUri
            (intIdx  :: Maybe Int) = R.readMaybe (T.unpack (T.takeWhileEnd (/='@') pkgUri))
            idxSt = PkgIdxTs $ fromMaybe 0 intIdx
        el "h2" $ text (pkgNToText pn)
        el "p" $ el "em" $ elAttr "a" ("href" =: ("https://hackage.haskell.org/package/" <> pkgNToText pn)) $
          do text "(view on Hackage)"

        evPB <- getPostBuild
        let 
        -- single-shot requests
        evReports <- getPackageReports (constDyn $ Right pn) evPB
        dynReports <- holdDyn mempty evReports

        evInfo <- getInfo evPB
        dynInfo <- holdDyn (ControllerInfo mempty) evInfo

        evHist <- getPackageHistory (constDyn $ Right pn) (leftmost [updated dynIdxStLast $> (), evPB])
        dynHist <- holdDyn mempty evHist

        evPkgTags <- getPackageTags (constDyn $ Right pn) evPB

        -- other requests
        evQRows <- getQueuePkg (constDyn $ Right pn) (leftmost [ticker4 $> (), evPB])
        dynQRows <- holdUniqDyn =<< holdDyn mempty evQRows

        evWorkers <- getWorkersPkg (constDyn $ Right pn) (leftmost [ticker2 $> (), evPB])
        dynWorkers <- holdUniqDyn =<< holdDyn mempty evWorkers


        _ <- el "p" $ do
          evQButton <- button "Queue a build"
          text " for latest index-state "
          dynText (pkgIdxTsToText <$> dynIdxStLast)

          putQueue (constDyn $ Right pn) (Right <$> dynIdxStLast) (constDyn $ Right (QEntryUpd (-1))) evQButton

        let inputAttr = ("class" =: "tag-name") <> ("placeholder" =: "insert tag")
            iCfg = TextInputConfig "tag-name" "" never (constDyn inputAttr)
        let xs = Map.fromList . fmap (\x -> (x, pkgIdxTsToText x)) . Set.toList <$> dynReports

        ddReports <- el "p" $ mdo
          let maxId = findInitialDropDown idxSt <$> dynReports
              ddCfg = (def :: DropdownConfig t PkgIdxTs)
                    & dropdownConfig_setValue .~ (updated maxId)
          initId <- sample $ current maxId
          evQButton <- button "Queue a build"
          text " for the index-state "
          dd <- dropdown initId xs ddCfg
          routePkgIdxTs pn dynReports (dd ^. dropdown_value)
          text " shown below"
          _ <- putQueue (constDyn $ Right pn) (Right <$> _dropdown_value dd) (constDyn $ Right (QEntryUpd (-1))) evQButton

          pure dd
        
        elClass "p" "tagging" $ mdo
          let evMapTags = Map.fromList . (fmap (\t -> (t,t))) . (fmap tagNToText) . V.toList <$> evPkgTags
          result <- foldDyn appEndo Map.empty $ fold
            [ Endo . const <$> evMapTags
            , (\nTag -> Endo $ Map.insert nTag nTag) <$> addTag0
            , (foldMap (Endo . Map.delete) . Map.keys) <$> deleteTag0
            ]
          deleteTag0 :: Event t (Map.Map T.Text T.Text) <- listViewWithKey result $ \tId _ -> do
            el "li" $ do
              el "span" $ text tId
              delEv <- rmTagButton_ tId pn
              pure $ tagNToText <$> delEv

          addTag0 <- elClass "form" "form" $ do
            el "p" $ text "Tag : "
            tagName <- textInput iCfg
            tagButton <- clickElement_ "button" "Add Tag"
            let tVal = _textInput_value tagName
                evAdd = (tagPromptlyDyn tVal tagButton)
            addTagN <- holdDyn "" evAdd
            addResult <- putTags ((Right . TagN) <$> addTagN) (constDyn $ Right pn) (() <$ evAdd)
            pure $ tagPromptlyDyn tVal addResult
          pure ()
        
        let dynIdxSt     = ddReports ^. dropdown_value
            evReports'   = updated (_dropdown_value ddReports)
            --evIdxChange  = updated dynIdxSt --ddReports ^. dropdown_change
        
        --display $ holdDyn (PkgIdxTs 0) evIdxChange
        evRepSum <- getPackageReportSummary (constDyn $ Right pn) (Right <$> dynIdxSt) (leftmost [evReports' $> (), ticker4 $> ()])
        dynRepSum <- holdUniqDyn =<< holdDyn (PkgIdxTsReport pn (PkgIdxTs 0) [] mempty) evRepSum
        
        el "hr" blank

        evCC <- dyn $ reportTableWidget <$> dynRepSum
                                        <*> dynQRows 
                                        <*> dynWorkers
                                        <*> dynHist 
                                        <*> dynInfo
        evCellClick <- switchHold never evCC
                        
        dynCellClick <- holdDyn Nothing (Just . _unCellTable <$> evCellClick) -- . _unCellTable 

        let dynCell' = mergeCellId pn <$> dynCellClick <*> dynIdxSt

        el "h2" (dynText $ ((maybe "No cell selected" (\(pn',pv,hcv,is) -> "Details for " <> pkgNToText pn' <> "-" <> verToText pv <> " / " <> compilerIdToText hcv <> " @ " <> pkgIdxTsToText is)) <$> dynCell'))

        reportDetailWidget dynCell'

        pure ()

    RouteUser u -> do
        el "h1" (text u)

        evPB <- getPostBuild

        evUserInfo <- getUser (constDyn $ Right u) evPB
        dynUserInfo <- holdDyn (UserPkgs u mempty) evUserInfo

        _ <- el "ol" $ simpleList ((V.toList . upPackages) <$> dynUserInfo) $ \pn -> do
          el "li" $ pkgLink pn

        pure ()

    RouteUnknown frag -> do
        el "p" $ text ("No handler found for " <> T.pack (show frag))
        pure ()

  pure ()
  where
    pkgLink pn' = elDynAttr "a" (pkgHref <$> pn') $ dynText (pkgNToText <$> pn')

    pkgHref (PkgN pn')
      | T.null pn' = mempty
      | otherwise  = ("href" =: ("#/package/" <> pn'))

    mergeCellId :: PkgN -> Maybe (Ver, CompilerID) -> PkgIdxTs -> Maybe (PkgN,Ver,CompilerID,PkgIdxTs)
    mergeCellId _ Nothing _           = Nothing
    mergeCellId pn (Just (pv,hcv)) is = Just (pn,pv,hcv,is)

    rmTagButton_ :: T.Text -> PkgN -> m (Event t TagN)
    rmTagButton_ tId pn = do
      rmTag <- do
        (ev1,_) <- elAttr' "a" ("class" =: "remove") $ do
                     text " X "
        pure $ domEvent Click ev1
      delResult <- deleteTags (constDyn $ Right (TagN tId)) (constDyn $ Right pn) rmTag
      pure $ (TagN tId) <$ delResult

-- | Renders alpha-tabbed package index
packagesPageWidget :: forall t m. (SetRoute t FragRoute m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m) 
                   => Dynamic t (Vector PkgN) 
                   -> Dynamic t (Vector TagN) 
                   -> Dynamic t (Map.Map PkgN [TagN])
                   -> m ()
packagesPageWidget dynPackages dynTags dynPkgTags = do
    display (V.length <$> dynPackages)

    dynTags' <- dyn $ do           
      v <- dynTags
      let v' = V.toList v 
      pure $ do 
        dynTagSet <- elClass "ol" "tag-filter clearfix" $ do
            result <- forM v' $ \(tn) -> do
                (ev1, _) <- el "li" $
                             elAttr' "a" ("class" =: "tag-item") $ do
                                text (tagNToText tn)
                pure $ tn <$ (domEvent Click ev1)
            pure $ leftmost result
        foldDyn toggleTagSet Set.empty dynTagSet
    dynSet' <- holdDyn (constDyn Set.empty) dynTags'
    let dynSet = join dynSet'

    dynPF <- el "div" $ do
      text "[ "
      eButton0 <- button "0-9"

      eButtons <- forM ['A'..'Z'] $ \c -> do
        text " | "
        button (T.singleton c)

      text " ]"
      holdDyn 'A' (leftmost [ e $> c | (e,c) <- zip (eButton0:eButtons) ('*':['A'..'Z']) ])

    -- this is faster than simpleList
    _ <- dyn $ do v <- dynPackages
                  pf <- dynPF
                  st <- dynSet
                  dpt <- dynPkgTags
                  let v' = V.toList . (evalTagFilter st dpt) . evalPkgFilter pf $ v

                  pure $ do

                    el "ol" $ forM_ v' $ \(pn) -> do
                      el "li" $ routeLink False ("#/package/" <> (pkgNToText pn)) $ do
                        text ((pkgNToText pn) <> " : ")
                        case Map.lookup pn dpt of
                          Just tags -> forM tags $ \(tag0) -> elAttr "a" (("class" =: "tag-item") <> ("data-tag-name" =: (tagNToText tag0))) $ text (tagNToText tag0)
                          Nothing   -> pure ([])
    pure ()
  where
    evalPkgFilter '*' = V.takeWhile (\(PkgN t) -> T.head t < 'A')
    evalPkgFilter c   = V.takeWhile f . V.dropWhile (not . f)
      where
        f (PkgN x) = let c' = T.head x in c' == c || c' == (C.toLower c)
    evalTagFilter st dpt pkg =
      V.filter (tagContained st dpt) pkg


reportTableWidget :: forall t m. ( SetRoute t FragRoute m, MonadHold t m, PostBuild t m, DomBuilder t m, Reflex t)
                  => PkgIdxTsReport
                  -> (Vector QEntryRow)
                  -> (Vector WorkerRow)
                  -> (Vector PkgHistoryEntry)
                  -> ControllerInfo
                  -> m (Event t CellTable)
reportTableWidget dynRepSum dynQRows dynWorkers dynHist dynInfo = do
  let PkgIdxTsReport{..} = dynRepSum
      qrowsAll = dynQRows
      wrowsAll = dynWorkers
      hrowsAll = dynHist
      ControllerInfo{..} = dynInfo
  
  let activeHcs = [ k | (k,CompilerInfo { ciActive = True }) <- Map.toDescList ciCompilers ]


  let (Just defVer) = verFromText "0"
      hcvsLR = computeLR pitrHcversions activeHcs
      hcvs   = applyLR hcvsLR pitrHcversions activeHcs

  let emptyHcvs   = hcvs      $> emptyCellReportSummary
      emptyActive = activeHcs $> emptyCellReportSummary

      vmap = Map.mergeWithKey (\_ (t,u) e -> Just (t,u,applyLR hcvsLR e emptyActive)) (fmap (\(t,u) -> (t,u,emptyHcvs))) (const mempty)
                                  vmap0 pitrPkgversions

      vmap0 = Map.fromList [ (v,(t,u)) | PkgHistoryEntry t v 0 u <- V.toList hrowsAll ]

  let inQueue = not (null [ () | QEntryRow{..} <- V.toList qrowsAll, qrIdxstate == pitrIdxstate ])

  let wip = [ (pv,hcv) | WorkerRow{..} <- V.toList wrowsAll
                           , wrIdxState == Just pitrIdxstate
                           , Just pv <- [wrPkgversion]
                           , Just hcv <- [wrHcversion]
                           ]

  let pn' = pkgNToText pitrPkgname

      -- TODO: push Dynamics into cells; the table dimensions are semi-static
  el "table" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" blank
        forM_ hcvs $ \cid -> el "th" (text (compilerIdToText cid))
        el "th" blank
        el "th" (text "released")
        el "th" (text "uploader")

    el "tbody" $ do 
      evrows <- sequence . List.reverse . snd $ List.mapAccumL (accumTableRow pn' hcvs pitrIdxstate wip inQueue) (defVer, ((PkgIdxTs 0), "", [])) (Map.toAscList vmap)
      pure (leftmost evrows)

accumTableRow :: forall t m. (SetRoute t FragRoute m, DomBuilder t m, Reflex t)
               => Text
               -> [CompilerID]
               -> PkgIdxTs
               -> [(Ver,CompilerID)]
               -> Bool
               -> (Ver, (PkgIdxTs, UserName, [CellReportSummary])) 
               -> (Ver, (PkgIdxTs, UserName, [CellReportSummary])) 
               -> ((Ver, (PkgIdxTs, UserName, [CellReportSummary])), m (Event t CellTable))
accumTableRow pn' hcvs pkgIdxTs wip inQ prevVer currVer = 
  let pkgVer              = const currVer prevVer
      (pcVer, (t, u, cs)) = pkgVer
      (ppVer, _)          = prevVer
  in (pkgVer, (renderRow pn' hcvs pkgIdxTs wip inQ ppVer pcVer t u cs))

renderRow :: forall t m. (SetRoute t FragRoute m, DomBuilder t m, Reflex t)
          => Text
          -> [CompilerID]
          -> PkgIdxTs
          -> [(Ver,CompilerID)]
          -> Bool
          -> (Ver)
          -> (Ver)
          -> PkgIdxTs
          -> UserName
          -> [CellReportSummary]
          -> m (Event t CellTable)
renderRow pn' hcvs pitrIdxstate wip inQueue ppV pcV t u cs = do
  let tooSoon = t > pitrIdxstate
      tooSoonAttr = if tooSoon then ("style" =: "opacity:0.5;") else mempty
  elAttr "tr" tooSoonAttr $ do
    elAttr "th" ("style" =: "text-align:left;") $ do
      let id2Ver = if verToText ppV == "0" then "" else "&id2=" <> verToText ppV
      elAttr "a" ("href" =: (mconcat [ "http://hdiff.luite.com/cgit/", pn', "/diff?id=",verToText pcV, id2Ver ])) $
        text "Δ"
      elAttr "a" ("href" =: (mconcat [ "https://hackage.haskell.org/package/", pn',"-",verToText pcV,"/",pn',".cabal/edit" ])) $
        text (verToText pcV)
    evsRow1 <- forM (zip cs hcvs) $ \(x,hcv) -> do
      let (cellAttr,cellText) = case crsT x of
            Nothing
              | tooSoon             -> ("class" =: "stat-unknown", el "b" (text ""))
              | (pcV,hcv) `elem` wip -> ("class" =: "stat-wip",     el "b" (text "WIP"))
              | inQueue             -> ("class" =: "stat-queued",  text "queued")
            _                       -> ("class" =: (snd $ fmtCRS x), (text $ fst $ fmtCRS x))

      (l,_) <- elAttr' "td" (("style" =: if tooSoon then "cursor: not-allowed;" else "cursor: cell;") <> cellAttr) cellText

      pure $ ((CellTable (pcV,hcv)) <$ (if tooSoon then never else domEvent Click l) :: Event t CellTable)

    elAttr "th" ("style" =: "text-align:left;") (text (verToText pcV))
    el "td" $ text (pkgIdxTsToText t)
    el "td" $ routeLink False ("#/user/" <> u) (text u)
    pure (leftmost evsRow1)

reportDetailWidget :: (SupportsServantReflex t m, MonadFix m, PostBuild t m, DomBuilder t m, Reflex t, MonadHold t m, Adjustable t m) => Dynamic t (Maybe (PkgN,Ver,CompilerID,PkgIdxTs)) -> m ()
reportDetailWidget dynCellId = do

    evDetails <- getPackageReportDetail (maybe (Left "") (Right . (^. _1)) <$> dynCellId)
                                                (maybe (Left "") (Right . (^. _4)) <$> dynCellId)
                                                (maybe (Left "") (Right . (^. _2)) <$> dynCellId)
                                                (maybe (Left "") (Right . (^. _3)) <$> dynCellId)
                                                (updated dynCellId $> ())

    dynRepTy <- holdDyn CRTna (crdType <$> evDetails)
    dynSErr  <- holdDyn "" ((fromMaybe "" . crdSolverErr) <$> evDetails)
    dynSols <- holdDyn [] ((fromMaybe [] . crdUnits) <$> evDetails)

    let h y x = if y==x then mempty else ("hidden" =: "")

    elDynAttr "div" (h CRTna <$> dynRepTy) $ do
      el "h3" (text "no reports yet")

    elDynAttr "div" (h CRTpf <$> dynRepTy) $ do
      el "h3" (text "No solutions found")
      el "pre" (dynText dynSErr)

    elDynAttr "div" (h CRTse <$> dynRepTy) $ do
      el "h3" (dynText $ (\xs -> tshow (length xs) <> " solution(s) found") <$> dynSols)

      _ <- simpleList dynSols $ \dynYs -> elAttr "div" ("style" =: "border-style: solid") $ do
        simpleList (Map.toList <$> dynYs) $ \dynY -> elAttr "div" ("style" =: "border-style: dotted") $ do
          evUpd <- getPostBuild

          el "h4" $ do
            el "tt" $ display (fst <$> dynY)
            text " "
            el "em" $ do
              text "["
              dynText (maybe "?" (T.drop 2 . tshow) . snd <$> dynY)
              text "]"

          evInfo <- getUnitInfo (Right . fst <$> dynY) evUpd

          dynLogmsg <- holdDyn "-" ((fromMaybe "" . uiiLogmsg) <$> evInfo)

          elDynAttr "pre" (st2attr . snd <$> dynY) (dynText dynLogmsg)

      pure ()

  where
    st2attr Nothing                = mempty
    st2attr (Just IPOk)            = mempty
    st2attr (Just IPBuildFail)     = ("style" =: "background-color: #ffdddd;")
    st2attr (Just IPBuildDepsFail) = mempty

    -- el "h2" (dynText $ ((maybe "No cell selected" (\(pv,hcv) -> "Details for " <> pn' <> "-" <> verToText pv <> " with " <> compilerIdToText hcv)) <$> dynCell))


{-

mkLink :: MonadWidget t m => T.Text -> T.Text -> m (Event t ())
mkLink s alt' = do
  (l,_) <- elAttr' "a" (("alt" =: alt') <> ("role" =: "button")) $ text s
  return $ domEvent Click l

mkButton :: MonadWidget t m => T.Text -> T.Text -> m (Event t ())
mkButton s alt' = do
  (l,_) <- elAttr' "button" (("alt" =: alt') <> ("style" =: "cursor:pointer;text-decoration:underline;")) $ text s
  return $ domEvent Click l

-}

mkWorkerStat :: (Vector WorkerRow) -> (PkgN -> PkgIdxTs -> Int)
mkWorkerStat ws = \x1 x2 -> Map.findWithDefault 0 (x1,x2) (Map.fromListWith (+) [ ((pn,is),1) | WorkerRow { wrPkgname = Just pn, wrIdxState = Just is } <- V.toList ws ])

tshow :: Show a => a -> Text
tshow = T.pack . show

data LR = L | R | LR

-- assumes strictly monotonic descending sequences
computeLR :: Ord a => [a] -> [a] -> [LR]
computeLR [] rs = rs $> R
computeLR ls [] = ls $> L
computeLR (l:ls) (r:rs) = case compare l r of
                            EQ -> LR : computeLR ls rs
                            GT -> L  : computeLR ls (r:rs)
                            LT -> R  : computeLR (l:ls) rs

-- left-biased
applyLR :: [LR] -> [a] -> [a] -> [a]
applyLR []      []     []     = []
applyLR (LR:xs) (l:ls) (_:rs) = l : applyLR xs ls rs
applyLR (L:xs)  (l:ls)    rs  = l : applyLR xs ls rs
applyLR (R:xs)     ls  (r:rs) = r : applyLR xs ls rs
applyLR _ _ _                 = error "applyLR"

findInitialDropDown :: PkgIdxTs -> Set PkgIdxTs -> PkgIdxTs
findInitialDropDown p s
  | True <- Set.null s
  = PkgIdxTs 0
  | otherwise = if Set.member p s then p else Set.findMax s
  {-if Set.null s 
  then PkgIdxTs 0
  else Set.findMax s-}

toggleTagSet :: TagN -> Set.Set TagN -> Set.Set TagN
toggleTagSet tn st = if Set.member tn st then Set.delete tn st else Set.insert tn st

tagContained :: Set.Set TagN -> Map.Map PkgN [TagN] -> PkgN -> Bool
tagContained st pkgTags pkg
  | Set.null st = True
  | otherwise    =
    let 
      tags = 
        case Map.lookup pkg pkgTags of
          Just a  -> a 
          Nothing -> []
        in not $ Set.null (Set.fromList tags `Set.intersection` st)

pkgTagList :: (Map.Map TagN (Vector PkgN))
           -> (Map.Map PkgN [TagN])
pkgTagList m = Map.fromListWith (List.++) $ do
  (k, vs) <- Map.toList m
  v <- (V.toList vs)
  pure $ (v, [k])

clickElement_ :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Text -> m (Event t ())
clickElement_ elm t = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
  (e, _) <- element elm cfg $ text t
  pure $ domEvent Click e

stripSearch :: JSS.JSString -> JSS.JSString
stripSearch sJ
  | Just sJ'  <- JSS.stripPrefix "^" sJ = sJ'
  | Just sJ' <- JSS.stripSuffix "$" sJ  = sJ'
  | otherwise                           = sJ

splitInfixPkg :: JSS.JSString -> JSS.JSString -> (Text, Text, Text)
splitInfixPkg stripSJ pkg = (frontT, midT, backT)
  where
    textS               = JSS.textFromJSString stripSJ
    (frontT, reminderT) = T.breakOn textS (JSS.textFromJSString pkg)
    (midT, backT)       = T.breakOnEnd textS reminderT

calcMatch :: JSS.JSString  -> JSS.JSString -> (Map.Map Text (), Map.Map Text (Text,Text,Text))
calcMatch sJss pkg
  | stripSJ == pkg = (Map.singleton (JSS.textFromJSString pkg) (), Map.empty)
  | otherwise      = filterPkgSearch sJss pkg
  where
    stripSJ = stripSearch sJss

filterPkgSearch :: JSS.JSString -> JSS.JSString -> (Map.Map Text (), Map.Map Text (Text,Text,Text))
filterPkgSearch sJss pkg
  | Just sJ' <- JSS.stripPrefix "^" sJss = if JSS.isPrefixOf sJ' pkg 
                                           then (Map.empty, Map.singleton (JSS.textFromJSString pkg) (splitInfixPkg sJ' pkg))
                                           else mempty
  | Just sJ' <- JSS.stripSuffix "$" sJss = if JSS.isSuffixOf sJ' pkg 
                                           then (Map.empty, Map.singleton (JSS.textFromJSString pkg) (splitInfixPkg sJ' pkg))
                                           else mempty
  | otherwise                            = if JSS.isInfixOf sJss pkg
                                           then (Map.empty, Map.singleton (JSS.textFromJSString pkg) (splitInfixPkg sJss pkg))
                                           else mempty

calcMatches :: [JSS.JSString] -> JSS.JSString -> Matches
calcMatches pkgs sJss
  | JSS.length sJss < 3  = matchesEmpty
  | otherwise            = Matches { matchesInput = textS, matchesExact = exactMap, matchesInfix = othersMap}
  where
    textS                = JSS.textFromJSString sJss
    (exactMap,othersMap) = F.foldMap (calcMatch sJss) pkgs

searchBoxWidget :: forall t m. (SetRoute t FragRoute m, SupportsServantReflex t m, MonadFix m, MonadIO m, MonadHold t m, PostBuild t m, DomBuilder t m, Adjustable t m, DomBuilderSpace m ~ GhcjsDomSpace)
                => Dynamic t (Vector PkgN) 
                -> m ()  
searchBoxWidget dynPkgs0 = mdo
  searchInputE <- elAttr "div" ("class" =: "item search right clearfix") $ do
    divClass "text" $ text "Package Search"
    sVal0 <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ fold ["class" =: "input-search","placeholder" =: "search..."]
                                & inputElementConfig_setValue .~ clickPkgE
    debounce 0.3 $ leftmost [clickPkgE, (_inputElement_input sVal0)]
  let
    dynPackagesJss = V.toList . fmap (JSS.textToJSString . pkgNToText) <$> dynPkgs0
    matchesE = calcMatches <$> current dynPackagesJss <@> (JSS.textToJSString <$> searchInputE)
  matchesDyn <- holdDyn matchesEmpty matchesE
  clickPkgE <- searchResultWidget matchesDyn
  pure ()

searchResultWidget :: forall t m. (SetRoute t FragRoute m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m) 
                   => Dynamic t Matches 
                   -> m (Event t Text)
searchResultWidget mDyn =
  el "ul" $ do
    exactE <- listViewWithKey (matchesExact <$> mDyn) $ \eId _ -> do
                (e, _) <- element "li" def $ 
                  routeLink False ("#/package/" <> eId) $ el "strong" $ text eId
                pure $ domEvent Click e
    otherE <- listViewWithKey (matchesInfix <$> mDyn) $ \pId txt -> do
                (e, _) <- element "li" def $ routeLink False ("#/package/" <> pId) $ do
                            dynText . fmap (^. _1) $ txt
                            el "strong" $ dynText . fmap (^. _2) $ txt
                            dynText . fmap (^. _3) $ txt
                pure $ domEvent Click e
    pure $ "" <$ leftmost [exactE, otherE]