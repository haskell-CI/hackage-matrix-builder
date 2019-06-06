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

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Main (main) where

import           Data.Aeson                (FromJSON)
import qualified Data.Aeson                as J
import qualified Data.Aeson.Types          as J
import qualified Data.Char                 as C
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import           Data.Time                 (getCurrentTime)
import           Data.Time.Clock.POSIX     (POSIXTime, posixSecondsToUTCTime,
                                            utcTimeToPOSIXSeconds)
import           Data.Time.Format          (defaultTimeLocale, formatTime)

import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Data.Version              as Ver
import           GHC.Generics              (Rep)
import           Network.URI
import           Reflex.Dom
import           Reflex.Dom.Contrib.Router (route)
import           Reflex.Dom.Location
-- import           Reflex.Dom.Routing.Nested
import           Control.Lens
import           Reflex.Dom.Widget.Basic
import           Reflex.Time
import           Servant.API
import           Servant.Reflex

import           API
import           PkgId


main :: IO ()
main = mainWidget bodyElement4

burl :: BaseUrl
burl | True      = BaseFullUrl Https "matrix.hackage.haskell.org" 443 "/api"
     | otherwise = BasePath "/api"

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

bodyElement4 :: forall t m . (SupportsServantReflex t m, MonadWidget t m) => m ()
bodyElement4 = do
    dynLoc <- browserHistoryWith getLocationUri
    let dynFrag = decodeFrag . T.pack . uriFragment <$> dynLoc

    -- ticker1 <- tickLossy 1 =<< liftIO getCurrentTime
--    ticker1cnt <- count ticker1

    -- top-level PB event
    evPB0 <- getPostBuild

    t0 <- liftIO getCurrentTime

    dynClock <- clockLossy 1 t0
    let dynUTCTime = _tickInfo_lastUTC <$> dynClock

    ticker2 <- tickLossy 2 t0
    let ticker4 = ffilter ((==0) . flip rem 2 . _tickInfo_n) ticker2
        ticker8 = ffilter ((==0) . flip rem 4 . _tickInfo_n) ticker2

    -- we can use this
    evIdxStLast <- fmapMaybe reqSuccess <$> getV2IdxStatesLatest (leftmost [ticker8 $> (), evPB0 $> ()])
    dynIdxStLast <- holdUniqDyn =<< holdDyn (PkgIdxTs 0) evIdxStLast

    evPackages0 <- fmapMaybe reqSuccess <$> getV2Packages (updated dynIdxStLast $> ())
    dynPackages0 <- holdDyn mempty evPackages0

    -- pseudo navbar
    el "nav" $ do
      text "[ "
      elAttr "a" ("href" =: "#/") $ text "HOME"
      text " | "
      elAttr "a" ("href" =: "#/queue") $ text "Build Queue"
      text " | "
      elAttr "a" ("href" =: "#/packages") $ text "Packages"
      text " ]"
      text "    (current index-state: "
      dynText (pkgIdxTsToText <$> dynIdxStLast)
      text " ; package count: "
      display (V.length <$> dynPackages0)
      text ")"

    el "hr" blank

    _ <- dyn $ dynFrag >>= \case
      RouteHome -> pure $ do
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

      RouteQueue -> pure $ do
        evPB <- getPostBuild

        let dynUnixTime = utc2unix <$> dynUTCTime

        evWorkers <- fmapMaybe reqSuccess <$> getV2Workers (leftmost [ticker2 $> (), evPB])
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
          evQRows <- fmapMaybe reqSuccess <$> getV2Queue (leftmost [ticker4 $> (), evPB])
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

          evHistRows <- fmapMaybe reqSuccess <$> getV2PackagesHistory (QParamSome <$> lb) (QParamSome <$> dynIdxStLast) (leftmost [updated dynIdxStLast $> (), evPB])
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

      RoutePackages -> pure $ do
          el "h1" $ text "Packages"
          packagesPageWidget dynPackages0

      RoutePackage pn -> pure $ do
          el "h2" $ text (unPkgN pn)
          el "p" $ el "em" $ elAttr "a" ("href" =: ("https://hackage.haskell.org/package/" <> unPkgN pn)) $
            do text "(view on Hackage)"

          evPB <- getPostBuild

          -- single-shot requests

          evReports <- fmapMaybe reqSuccess <$> getV2PackageReports  (constDyn $ Right pn) evPB
          dynReports <- holdDyn mempty evReports

          evInfo <- fmapMaybe reqSuccess <$> getV2Info evPB
          dynInfo <- holdDyn (ControllerInfo mempty) evInfo

          evHist  <- fmapMaybe reqSuccess <$> getV2PackageHistory (constDyn $ Right pn) (leftmost [updated dynIdxStLast $> (), evPB])
          dynHist <- holdDyn mempty evHist

          -- other requests

          evQRows <- (fmapMaybe reqSuccess) <$>
                     getV2QueuePkg (constDyn $ Right pn) (leftmost [ticker4 $> (), evPB])
          dynQRows <- holdUniqDyn =<< holdDyn mempty evQRows

          evWorkers <- (fmapMaybe reqSuccess) <$>
                       getV2WorkersPkg (constDyn $ Right pn) (leftmost [ticker2 $> (), evPB])
          dynWorkers <- holdUniqDyn =<< holdDyn mempty evWorkers


          _ <- el "p" $ do
            evQButton <- button "Queue a build"
            text " for latest index-state "
            dynText (pkgIdxTsToText <$> dynIdxStLast)

            putV2Queue (constDyn $ Right pn) (Right <$> dynIdxStLast) (constDyn $ Right (QEntryUpd (-1))) evQButton


          let xs = Map.fromList . fmap (\x -> (x, pkgIdxTsToText x)) . Set.toList <$> dynReports
              x0 = (\s -> if Set.null s then PkgIdxTs 0 else Set.findMax s) <$> dynReports

          let cfg = DropdownConfig (updated x0) (constDyn mempty)

          ddReports <- el "p" $ do
            evQButton <- button "Queue a build"
            text " for the index-state "
            tmp <- dropdown (PkgIdxTs 0) xs cfg
            text " shown below"

            _ <- putV2Queue (constDyn $ Right pn) (Right <$> _dropdown_value tmp) (constDyn $ Right (QEntryUpd (-1))) evQButton

            pure tmp

          let evReports' = updated (_dropdown_value ddReports)
              dynIdxSt   = ddReports ^. dropdown_value

          evRepSum <- fmapMaybe reqSuccess <$> getV2PackageReportSummary (constDyn $ Right pn) (Right <$> dynIdxSt) (leftmost [evReports' $> (), ticker4 $> ()])

          dynRepSum <- holdUniqDyn =<< holdDyn (PkgIdxTsReport pn (PkgIdxTs 0) [] mempty) evRepSum


          el "hr" blank

          evCellClick <- reportTableWidget dynRepSum dynQRows dynWorkers dynHist dynInfo

          dynCellClick <- holdDyn Nothing (Just <$> evCellClick)

          let dynCell' = mergeCellId pn <$> dynCellClick <*> dynIdxSt

          el "h2" (dynText $ ((maybe "No cell selected" (\(pn',pv,hcv,is) -> "Details for " <> pkgNToText pn' <> "-" <> verToText pv <> " / " <> compilerIdToText hcv <> " @ " <> pkgIdxTsToText is)) <$> dynCell'))

          reportDetailWidget dynCell'

          pure ()

      RouteUser u -> pure $ do
          el "h1" (text u)

          evPB <- getPostBuild

          evUserInfo <- fmapMaybe reqSuccess <$> getV2User (constDyn $ Right u) evPB
          dynUserInfo <- holdDyn (UserPkgs u mempty) evUserInfo

          _ <- el "ol" $ simpleList ((V.toList . upPackages) <$> dynUserInfo) $ \pn -> do
            el "li" $ pkgLink pn

          pure ()

      RouteUnknown frag -> pure $ do
          el "p" $ text ("No handler found for " <> T.pack (show frag))
          pure ()

    pure ()
  where
    ClientFuns{..} = mkClientFuns burl

    unPkgN (PkgN x) = x

    pkgLink pn' = elDynAttr "a" (pkgHref <$> pn') $ dynText (unPkgN <$> pn')

    pkgHref (PkgN pn')
      | T.null pn' = mempty
      | otherwise  = ("href" =: ("#/package/" <> pn'))


    mergeCellId :: PkgN -> Maybe (Ver, CompilerID) -> PkgIdxTs -> Maybe (PkgN,Ver,CompilerID,PkgIdxTs)
    mergeCellId _ Nothing _           = Nothing
    mergeCellId pn (Just (pv,hcv)) is = Just (pn,pv,hcv,is)


data FragRoute = RouteHome
               | RouteQueue
               | RoutePackages
               | RoutePackage PkgN
               | RouteUser UserName
               | RouteUnknown T.Text
               deriving (Eq)

decodeFrag :: T.Text -> FragRoute
decodeFrag frag = case frag of
    ""           -> RouteHome
    "#"          -> RouteHome
    "#/"         -> RouteHome
    "#/queue"    -> RouteQueue
    "#/packages" -> RoutePackages

    _ | Just sfx <- T.stripPrefix "#/package/" frag
      , not (T.null frag)
      , Just pn <- pkgNFromText sfx
        -> RoutePackage pn

      | Just sfx <- T.stripPrefix "#/user/" frag
      , not (T.null frag)
      , T.all (\c -> C.isAsciiLower c || C.isAsciiUpper c || C.isDigit c || c == '_') sfx
        -> RouteUser sfx

      | otherwise -> RouteUnknown frag


-- | Renders alpha-tabbed package index
packagesPageWidget :: (MonadHold t m, PostBuild t m, DomBuilder t m) => Dynamic t (Vector PkgN) -> m ()
packagesPageWidget dynPackages = do
    display (V.length <$> dynPackages)

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
                  let v' = V.toList . evalPkgFilter pf $ v

                  pure $ do
                    el "ol" $ forM_ v' $ \(PkgN pn) -> do
                      el "li" $ elAttr "a" ("href" =: ("#/package/" <> pn)) $ text pn

    pure ()
  where
    evalPkgFilter '*' = V.takeWhile (\(PkgN t) -> T.head t < 'A')
    evalPkgFilter c   = V.takeWhile f . V.dropWhile (not . f)
      where
        f (PkgN x) = let c' = T.head x in c' == c || c' == (C.toLower c)


reportTableWidget :: forall t m . (MonadWidget t m, MonadHold t m, PostBuild t m, DomBuilder t m)
                  => Dynamic t PkgIdxTsReport
                  -> Dynamic t (Vector QEntryRow)
                  -> Dynamic t (Vector WorkerRow)
                  -> Dynamic t (Vector PkgHistoryEntry)
                  -> Dynamic t ControllerInfo
                  -> m (Event t (Ver,CompilerID))
reportTableWidget dynRepSum dynQRows dynWorkers dynHist dynInfo = joinE =<< go
  where
    go = dyn $ do
      PkgIdxTsReport{..} <- dynRepSum
      qrowsAll <- dynQRows
      wrowsAll <- dynWorkers
      hrowsAll <- dynHist
      ControllerInfo{..} <- dynInfo
      let activeHcs = [ k | (k,CompilerInfo { ciActive = True }) <- Map.toDescList ciCompilers ]


      let hcvsLR = computeLR pitrHcversions activeHcs
          hcvs   = applyLR hcvsLR pitrHcversions activeHcs

      let vmap = Map.mergeWithKey (\_ (t,u) e -> Just (t,u,applyLR hcvsLR e emptyActive)) (fmap (\(t,u) -> (t,u,emptyHcvs))) (const mempty)
                                  vmap0 pitrPkgversions

          emptyHcvs   = hcvs      $> emptyCellReportSummary
          emptyActive = activeHcs $> emptyCellReportSummary

          vmap0 = Map.fromList [ (v,(t,u)) | PkgHistoryEntry t v 0 u <- V.toList hrowsAll ]

      let inQueue = not (null [ () | QEntryRow{..} <- V.toList qrowsAll, qrIdxstate == pitrIdxstate ])

      let wip = [ (pv,hcv) | WorkerRow{..} <- V.toList wrowsAll
                           , wrIdxState == Just pitrIdxstate
                           , Just pv <- [wrPkgversion]
                           , Just hcv <- [wrHcversion]
                           ]

      let pn' = pkgNToText pitrPkgname

      -- TODO: push Dynamics into cells; the table dimensions are semi-static
      pure $ do
        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" blank
              forM_ hcvs $ \cid -> el "th" (text (compilerIdToText cid))
              el "th" blank
              el "th" (text "released")
              el "th" (text "uploader")

          el "tbody" $ do
            evsRows <- forM (Map.toDescList vmap) $ \(pv,(t,u,cs)) -> do
              let tooSoon = t > pitrIdxstate
                  tooSoonAttr = if tooSoon then ("style" =: "opacity:0.5;") else mempty
              elAttr "tr" tooSoonAttr $ do
                elAttr "th" ("style" =: "text-align:left;") $
                  elAttr "a" ("href" =: (mconcat [ "https://hackage.haskell.org/package/", pn',"-",verToText pv,"/",pn',".cabal/edit" ])) $
                    text (verToText pv)
                evsRow1 <- forM (zip cs hcvs) $ \(x,hcv) -> do
                  let (cellAttr,cellText) = case crsT x of
                        Nothing
                          | tooSoon             -> ("class" =: "stat-unknown", el "b" (text ""))
                          | (pv,hcv) `elem` wip -> ("class" =: "stat-wip",     el "b" (text "WIP"))
                          | inQueue             -> ("class" =: "stat-queued",  text "queued")
                        _                       -> ("class" =: (snd $ fmtCRS x), (text $ fst $ fmtCRS x))

                  (l,_) <- elAttr' "td" (("style" =: if tooSoon then "cursor: not-allowed;" else "cursor: cell;") <> cellAttr) cellText

                  pure $ ((pv,hcv) <$ (if tooSoon then never else domEvent Click l) :: Event t (Ver,CompilerID))

                elAttr "th" ("style" =: "text-align:left;") (text (verToText pv))
                el "td" $ text (pkgIdxTsToText t)
                el "td" $ elAttr "a" ("href" =: ("#/user/" <> u)) (text u)

                pure (leftmost evsRow1)
            pure (leftmost evsRows) -- main "return" value




reportDetailWidget :: (SupportsServantReflex t m, MonadWidget t m) => Dynamic t (Maybe (PkgN,Ver,CompilerID,PkgIdxTs)) -> m ()
reportDetailWidget dynCellId = do

    evDetails <- fmapMaybe reqSuccess <$> getV2PackageReportDetail (maybe (Left "") (Right . (^. _1)) <$> dynCellId)
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

          evInfo <- fmapMaybe reqSuccess <$> getV2UnitInfo (Right . fst <$> dynY) evUpd

          dynLogmsg <- holdDyn "-" ((fromMaybe "" . uiiLogmsg) <$> evInfo)

          elDynAttr "pre" (st2attr . snd <$> dynY) (dynText dynLogmsg)

      pure ()

  where
    st2attr Nothing                = mempty
    st2attr (Just IPOk)            = mempty
    st2attr (Just IPBuildFail)     = ("style" =: "background-color: #ffdddd;")
    st2attr (Just IPBuildDepsFail) = mempty

    ClientFuns{..} = mkClientFuns burl


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


tshow :: Show a => a -> T.Text
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



joinE :: forall t m a . (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
joinE = fmap switch . hold never
