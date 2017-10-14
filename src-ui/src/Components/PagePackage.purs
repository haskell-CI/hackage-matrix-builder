module Components.PagePackage where

import CSS.Display as D
import Control.Monad.Eff.Exception as E
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.Types as T
import Network.RemoteData as RD
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, isNothing)
import Data.Traversable (Accum, mapAccumL)
import Lib.MiscFFI as Misc
import Prelude (type (~>), Unit, unit, bind, negate, const, discard, otherwise, pure, show, (<), (>), ($), (&&), (/=), (<$>), (<>), (==), (||), (>>=), (-), (+), (<<<))
import DOM.HTML (window) as DOM
import DOM.HTML.History (DocumentTitle(DocumentTitle), URL(URL), pushState) as DOM
import DOM.HTML.Window (history, scroll, screenY) as DOM
import Debug.Trace
import Data.Tuple as Tuple
import Data.Tuple.Nested as TupleN
import Data.Foldable as Foldable
import Data.Argonaut as Arg
import Data.Foreign as F
import Data.StrMap as SM
import Data.Int as Int
import Data.Map as Map
import Data.Ordering (Ordering)
import Data.Ord (compare)


type State =
  { initPackage :: T.InitialPackage
  , logdisplay  :: D.Display
  , report      :: RD.RemoteData E.Error T.PackageIdxTsReports
  , history     :: Array (Tuple.Tuple T.VersionName T.Revision)
  , highlighted :: Boolean
  , logmessage  :: String
  , columnversion :: T.ColumnVersion
  , newtag :: T.TagName
  , newPrio :: Int
  , queueStatus :: RD.RemoteData E.Error T.PackageQueue
  , queueClicked :: Boolean
  , listTimeStamp :: Array T.PkgIdxTs
  , currentSelectedIdx :: T.PkgIdxTs
  , latestIndex :: T.PackageTS
  , mapIndexState :: Map.Map Int T.PackageTS
  , listTags :: RD.RemoteData E.Error (Array T.TagName)
  , currKey :: Int
  , unitsCell :: Array (SM.StrMap T.BuildStatus)
  , currCellSum :: Tuple.Tuple T.HCVer T.CellReportSummary
  }

data Query a
  = Initialize a
  | FailingPackage a
  | HighlightSE (Tuple.Tuple String T.BuildStatus) a
  | HighlightCell T.PackageIdxTsReports T.CellReportSummary (Maybe T.HCVer) (Maybe T.VersionName) a
  | HandleTag T.TagName a
  | Receive T.InitialPackage a
  | AddingNewTag T.PackageName T.TagName a
  | RemoveTag T.PackageName T.TagName a
  | UpdateTag (T.PackageName -> a)
  | HandleQueue Int a
  | QueueBuild T.PackageName T.PkgIdxTs Int (RD.RemoteData E.Error T.PackageQueue) a
  | HandleIndex State Int a
  | Finalize a

data Message = FromPagePackage

component :: forall e.
             H.Component HH.HTML Query T.InitialPackage Message (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: HE.input Receive
  }
  where

    initialState :: T.InitialPackage -> State
    initialState i =
      {
        initPackage: i
      , logdisplay: D.displayNone
      , report: RD.NotAsked
      , history: []
      , highlighted: false
      , logmessage: ""
      , columnversion: { ghcVer: Nothing
                       , pkgVer: Nothing
                       }
      , newtag: ""
      , newPrio: 0
      , queueStatus: RD.NotAsked
      , queueClicked: false
      , listTimeStamp: []
      , currentSelectedIdx: 0
      , latestIndex: ""
      , mapIndexState: Map.empty
      , listTags: RD.NotAsked
      , currKey: 0
      , unitsCell: []
      , currCellSum: Tuple.Tuple "" T.cellReportSummaryDefault
      }

    render :: State -> H.ComponentHTML Query
    render state = renderBody' state state.report

      where
        renderBody' state (RD.Success shallowR) =
          HH.div
            [ HP.id_ "page-package"
            , HP.class_ (H.ClassName "page")
            ]
            [ HH.div
                [ HP.class_ (H.ClassName "rightcol") ]
                [ legend
                , queueing state
                , tagging state
                , indexing state
                ]
            , HH.div
                [ HP.class_ (H.ClassName "leftcol") ] $
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text $ TupleN.get1 state.initPackage ]
                , HH.div
                    [ HP.id_ "package-buildreport" ] $
                    [ HH.h3
                        [ HP.class_ (H.ClassName "package-header") ]
                        [ HH.text "Solver Matrix (constrained by single version) " ]
                    , HH.div
                        [ HP.id_ "indexing" ] $
                        renderNavBtn state
                    , HH.div
                        [ HP.id_ "package" ] [ (renderTableMatrix state) ]
                    , HH.h3
                        [ HP.class_ (H.ClassName "logs-header") ]
                        [ HH.text "Logs" ]
                    ]
                    <> (if Arr.null state.unitsCell then [] else renderUnitsButton state state.unitsCell state.columnversion )
                    <> [ HH.div
                           [ HP.id_ "tabs-container" ]
                           [ (renderLogResult state.logdisplay state.logmessage state.report state.columnversion) ]
                       ]
                ]
            ]
        renderBody' state RD.NotAsked =
          HH.div
            [ HP.id_ "page-package"
            , HP.class_ (H.ClassName "page")
            ]
            [ HH.div
                [ HP.class_ (H.ClassName "rightcol") ]
                [ legend
                , queueing state
                , tagging state
                ]
            , HH.div
                [ HP.class_ (H.ClassName "leftcol") ]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text $ "" ]
                ]
            ]

        renderBody' state _ =
          HH.div
            [ HP.id_ "page-package"
            , HP.class_ (H.ClassName "page")
            ]
            [ HH.div
                [ HP.class_ (H.ClassName "rightcol") ]
                [ legend
                , queueing state
                , tagging state
                ]
            , HH.div
                [ HP.class_ (H.ClassName "leftcol") ]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text $ ""]
                , HH.div
                    [ HP.classes (H.ClassName <$> ["main-header-subtext","error"]) ]
                    [ HH.text $ "This package doesn't have a build report yet. You can request a report by"
                    , HH.a [ HP.href "https://github.com/hvr/hackage-matrix-builder/issues/32" ] [ HH.text " leaving a comment here" ]
                    ]
                , HH.div
                    [ HP.id_ "package-buildreport" ]
                    [ HH.h3
                        [ HP.class_ (H.ClassName "package-header") ]
                        [ HH.text "Solver Matrix (constrained by single version) " ]
                    , HH.div
                        [ HP.id_ "indexing" ] $
                        renderNavBtn state
                    , HH.div
                        [ HP.id_ "package" ]
                        [ ]
                    , HH.h3
                        [ HP.class_ (H.ClassName "logs-header") ]
                        [ HH.text "Logs" ]
                    , HH.div
                        [ HP.id_ "tabs-container" ]
                        [ ]
                    ]
                ]
            ]
        tagging st =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Tags" ]
            , HH.div
                [ HP.id_ "tagging" ]
                [ HH.ul
                    [ HP.class_ (H.ClassName "tags") ] $
                    renderPackageTag (TupleN.get1 st.initPackage) st.listTags st.newtag
                , HH.div
                    [ HP.class_ (H.ClassName "form") ]
                    [ HH.label_
                        [ HH.text "Tag"
                        , HH.input
                            [ HP.type_ HP.InputText
                            , HP.class_ (H.ClassName "tag-name")
                            , HE.onValueInput (HE.input HandleTag)
                            ]

                        ]
                    , HH.button
                        [ HP.class_ (H.ClassName "action")
                        , HE.onClick $ HE.input_ (AddingNewTag st.newtag
                                                               (TupleN.get1 st.initPackage))
                        ]
                        [ HH.text "Add Tag" ]
                     ]
                ]
            ]
        queueing st =
          HH.div
            [ HP.class_ (H.ClassName "sub") ] $
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Queueing" ]
            ] <> (checkQueueStatus st.queueStatus) <>
            [ HH.div
                [ HP.id_ "queueing" ] $
                if st.queueClicked then buildIsQueued else generateQueueButton st
            ]

        indexing st =
          HH.div
            [ HP.class_ (H.ClassName "sub") ] $
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Index State"]
            , HH.div
                [ HP.id_ "indexing" ] $
                if Arr.null st.listTimeStamp then timeStampIsEmpty else generateIndexStateButton st
            ]

    eval :: Query ~> H.ComponentDSL State Query Message (Api.Matrix e)
    eval (Initialize next) = do
      st <- H.get
      listIndex <- Api.getPackageReports  (TupleN.get1 st.initPackage)
      tags <- Api.getPackageTags (TupleN.get1 st.initPackage)
      historyPackage <- H.lift $ Api.getPackageHistories (TupleN.get1 st.initPackage)
      traceAnyA (TupleN.get1 st.initPackage)
      traceAnyA (TupleN.get2 st.initPackage)
      let
        listIndex' =
          case listIndex of
            RD.Success idx' -> idx'
            _               -> []
        latestIdx = Misc.getLastIdx listIndex'
        selectedIdx = getIdx (TupleN.get2 st.initPackage) listIndex'
        mapIdxSt = Map.fromFoldable (toTupleArray listIndex)
        maxKey =
          case Map.findMax mapIdxSt of
            Just { key } -> (key :: Int)
            Nothing      -> 0
        hist = case historyPackage of
          RD.Success hs -> Arr.sortBy sortVer (Arr.zip (TupleN.get2 <$> hs) (TupleN.get3 <$> hs))
          _             -> []
      reportPackage <- H.lift $ Api.getPackageIdxTsReports (TupleN.get1 st.initPackage) selectedIdx
      queueStat <- H.lift $ Api.getSpecificQueue (TupleN.get1 st.initPackage) selectedIdx
      H.modify _  { report = reportPackage
                  , history = hist
                  , highlighted = false
                  , queueStatus = queueStat
                  , listTimeStamp = listIndex'
                  , currentSelectedIdx = selectedIdx
                  , latestIndex = show latestIdx
                  , mapIndexState = mapIdxSt
                  , listTags = tags
                  , currKey = maxKey
                  }
      pure next
      where
        initpkgname st = TupleN.get1 st.initPackage
        initpkgidx st = TupleN.get2 st.initPackage
        initpkgver st = TupleN.get3 st.initPackage
        inithcver st = TupleN.get4 st.initPackage
    eval (FailingPackage next) = do
      pure next
    eval (HighlightSE (Tuple.Tuple un status) next) =
      if status == "bok" || status == "bfail"
      then do
        unitsLog <- H.lift $ Api.getUnitIdInfo un
        let
          unitId = case unitsLog of
            RD.Success a -> a
            _            -> T.unitIdInfoEmpty
        H.modify _ { logmessage = unitId.uiiLogmsg}
        pure next
      else
        pure next
    eval (HighlightCell {pkgname, idxstate} summ ghcV pkgV next) = do
      singleResult <- H.lift $ Api.getCellReportDetail pkgname idxstate pkgVer' ghcVer'
      let
        sRU = case singleResult of
               RD.Success a -> a.units
               _            -> []
        sRS = case singleResult of
               RD.Success a -> a.solverE
               _            -> ""
        arrKeys = Arr.concat (SM.keys <$> sRU)
        logs = case summ.crsT of
          "pf" -> sRS
          "se" -> "There is exist at least one build log. Please click the button above"
          _    -> ""
        sObj = SM.singleton "name" (Arg.encodeJson pkgname)
        jObj = F.toForeign (SM.insert "index" (Arg.encodeJson idxstate) sObj)
        indexURL = if Str.null idxstate' then "" else "@" <> idxstate'
        ghcURL = if isNothing ghcV then "" else "/" <> fromMaybe "" ghcV
        pkgverURL = if isNothing pkgV then "" else "/" <> fromMaybe "" pkgV
        pageName = DOM.DocumentTitle $ pkgname <> " - " <> idxstate'
        pageUrl = DOM.URL $ "#/package/" <> pkgname <> pkgverURL <> ghcURL <> indexURL

      H.modify _ { logmessage = logs
                 , logdisplay = if Str.null logs then D.displayNone else D.block
                 , columnversion = { ghcVer: ghcV, pkgVer: pkgV }
                 , unitsCell = sRU
                 , currCellSum = Tuple.Tuple ghcVer' summ
                 }
      heightPage <- H.liftEff $ DOM.window >>= Misc.scrollMaxY
      _ <- H.liftEff $ DOM.window >>= (DOM.scroll 0 heightPage)
      traceAnyA sRU
      hist <- H.liftEff $ DOM.window >>= DOM.history
      pushS <- H.liftEff $ DOM.pushState jObj pageName pageUrl hist

      pure next
      where
        idxstate' = show idxstate
        ghcVer' = fromMaybe "" ghcV
        pkgVer' = fromMaybe "" pkgV
    eval (HandleTag value next) = do
      st <- H.get
      H.put $ st { newtag = value }
      pure next
    eval (AddingNewTag newTag pkgName next) = do
      _ <- H.lift $ Api.putPackageTag newTag pkgName
      H.raise $ FromPagePackage
      pure next
    eval (RemoveTag tagName pkgName next) = do
      _ <- H.lift $ Api.deletePackageTag tagName pkgName
      H.raise $ FromPagePackage
      pure next
    eval (UpdateTag reply) = do
      st <- H.get
      let packageName = Tuple.fst st.initPackage
      reply <$> (pure packageName)
    eval (Receive pkg next) = do
      st <- H.get
      listIndex <- Api.getPackageReports (initpkgname pkg)
      tags <- Api.getPackageTags (initpkgname pkg)
      historyPackage <- H.lift $ Api.getPackageHistories (initpkgname pkg)
      let
        listIndex' =
          case listIndex of
            RD.Success idx' -> idx'
            _              -> []
        latestIdx = Misc.getLastIdx listIndex'
        selectedIdx = getIdx (initpkgidx pkg) listIndex'
        mapIdxSt = Map.fromFoldable (toTupleArray listIndex)
        urlPkgVer = TupleN.get3 pkg
        urlGhcVer = TupleN.get4 pkg
        maxKey =
          case Map.findMax mapIdxSt of
            Just { key } -> (key :: Int)
            Nothing      -> 0
        hist = case historyPackage of
          RD.Success hs -> Arr.sortBy sortVer $ Arr.zip (TupleN.get2 <$> hs) (TupleN.get3 <$> hs)
          _             -> []
      reportPackage <- H.lift $ Api.getPackageIdxTsReports (initpkgname pkg) selectedIdx
      queueStat <- H.lift $ Api.getSpecificQueue (initpkgname pkg) selectedIdx
      traceAnyA "from receive"

      H.modify _  { initPackage = pkg
                  , report = reportPackage
                  , history = hist
                  , highlighted = false
                  , queueStatus = queueStat
                  , listTimeStamp = listIndex'
                  , currentSelectedIdx = selectedIdx
                  , latestIndex = show latestIdx
                  , mapIndexState = mapIdxSt
                  , listTags = tags
                  , currKey =
                      case Arr.elemIndex (initpkgidx pkg) (show <$> listIndex') of
                        Just a  -> a
                        Nothing -> maxKey
                  }
      if isNothing urlPkgVer || isNothing urlGhcVer
        then
          pure next
        else do
          let
            currPkgVer = fromMaybe "" urlPkgVer
            currGhcVer = fromMaybe "" urlGhcVer
            currReport =
              case reportPackage of
                RD.Success a -> a
                _            -> T.pkgIdxTsReportsEmpty
            currPkgVersion =
              case SM.lookup currPkgVer currReport.pkgversions of
                Just arr -> arr
                Nothing  -> []
            ghcIndex =
              case Arr.elemIndex currGhcVer currReport.hcversions of
                Just idx -> idx
                Nothing -> 0
            ghcSumm =
              case Arr.index currPkgVersion ghcIndex of
                Just summ -> summ
                Nothing   -> T.cellReportSummaryDefault
          eval (HighlightCell currReport ghcSumm urlGhcVer urlPkgVer next)
      where
        initpkgname st = TupleN.get1 st
        initpkgidx st = TupleN.get2 st
        initpkgver st = TupleN.get3 st
        inithcver st = TupleN.get4 st

    eval (HandleQueue idx next) = do
      st <- H.get
      _ <- case idx of
              1 -> H.modify _ { newPrio = 0}
              2 -> H.modify _ { newPrio = (negate 10)}
              _ -> H.modify _ { newPrio = 10}
      pure next
    eval (QueueBuild pkgName currIdx prio (RD.Success queue) next ) = do
      _ <- H.lift $ Api.putPackageQueue pkgName queue.idxstate prio
      H.modify _ { queueClicked = true }
      pure next
    eval (QueueBuild pkgName currIdx prio _ next) = do
      H.modify _ { queueClicked = true }
      _ <- H.lift $  Api.putPackageQueue pkgName currIdx prio
      pure next
    eval (HandleIndex {initPackage, mapIndexState, columnversion, currCellSum} idx next) = do
      if emptybuild
       then H.modify _ { logmessage = ""
                      , logdisplay =  D.displayNone
                      , columnversion = { ghcVer: Nothing, pkgVer: Nothing }
                      , currKey = idx
                      }
       else H.modify _ { currKey = idx }
      let
        selectedIndex' =
          case Map.lookup idx mapIndexState of
            Just a -> a
            Nothing -> ""
        package = TupleN.get1 initPackage
      eval (Receive (TupleN.tuple4 package selectedIndex' pkgver ghcver) next)
      where
        ghcver = columnversion.ghcVer
        pkgver = columnversion.pkgVer
        emptyghcpkg =  isNothing ghcver || isNothing pkgver
        emptybuild = case _.crsT (Tuple.snd currCellSum) of
          "na" -> true
          ""   -> true
          _    -> false
    eval (Finalize next) = do
      pure next

checkQueueStatus :: forall p i. RD.RemoteData E.Error T.PackageQueue ->  Array (HH.HTML p i)
checkQueueStatus (RD.Success qi) =
  [ HH.div
      [ HP.class_ (H.ClassName "already-queued") ]
      [ HH.text ("This package is already in " <>
                 Misc.showPrio qi.priority <>
                 " priority queue. You can change its priority queue below.")
      ]
  ]
checkQueueStatus _              = []

buildIsQueued :: forall p i. Array (HH.HTML p i)
buildIsQueued =
  [ HH.div
      [ HP.class_ (H.ClassName "success") ]
      [ HH.text "Build queued!" ]
  ]

generateQueueButton :: forall p. State -> Array (HH.HTML p (Query Unit))
generateQueueButton st =
  [ HH.div
      [ HP.class_ (H.ClassName "form") ]
        [ HH.label_
            [ HH.text "Priority"
            , HH.select
                [ HP.class_ (H.ClassName "prio")
                , HE.onSelectedIndexChange $ HE.input HandleQueue
                ] $
                [ HH.option
                    ([ HP.value "high" ] <> isPrioSelected 10 st.queueStatus)
                    [ HH.text "High" ]
                , HH.option
                    ([ HP.value "medium" ] <> isPrioSelected 0 st.queueStatus)
                    [ HH.text "Medium" ]
                , HH.option
                    ([ HP.value "low" ] <> isPrioSelected (negate 10) st.queueStatus)
                    [ HH.text "Low" ]
                ]
            ]
        , HH.button
            [ HP.class_ (H.ClassName "action")
            , HE.onClick $ HE.input_ (QueueBuild (Tuple.fst st.initPackage) st.currentSelectedIdx st.newPrio st.queueStatus)
            ]
            [ HH.text "Queue build for this package" ]
        ]
  ]


toTupleArray :: RD.RemoteData E.Error (Array T.PkgIdxTs) -> Array (Tuple.Tuple Int T.PackageTS)
toTupleArray (RD.Success xs) =
  let len = (Arr.length xs) - 1
      idx = Arr.(..) 0 len
  in Arr.zip idx (show <$> xs)
toTupleArray _ = []

createIndexOption :: forall p i. State -> T.PkgIdxTs -> HH.HTML p i
createIndexOption st idx' =
  let
    idx = show idx'
  in
     HH.option
       ([ HP.value idx ] <> isIndexSelected st idx )
       [ HH.text (Misc.toDateTime idx') ]

isIndexSelected :: forall p i. State
                -> T.PackageTS
                -> Array (HH.IProp (selected :: Boolean | p) i)
isIndexSelected st idx =
  case (show st.currentSelectedIdx) == idx of
    true  -> [HP.selected true]
    false -> if Str.null (show st.currentSelectedIdx) && st.latestIndex == idx
                then [HP.selected true]
                else []

isPrioSelected :: forall p i. Int
               -> RD.RemoteData E.Error T.PackageQueue
               -> Array (HH.IProp (selected :: Boolean | p) i)
isPrioSelected prio (RD.Success qi) =
  case prio == qi.priority of
    true -> [HP.selected true]
    false -> []
isPrioSelected prio _                      = []

renderMissingPackage :: forall p i. T.PackageName -> HH.HTML p i
renderMissingPackage pkgName =
  HH.div
    [ HP.classes (H.ClassName <$> ["main-header-subtext","error"])
    , HP.id_ "package-not-built"
    ]
    [ HH.text "This package doesn't have a build report yet. You can request a report by"
    , HH.a
        [ HP.href "https://github.com/hvr/hackage-matrix-builder/issues/32"]
        [ HH.text "leaving a comment here"]
    ]

renderLogResult :: forall p i. D.Display -> String -> RD.RemoteData E.Error T.PackageIdxTsReports -> T.ColumnVersion -> HH.HTML p i
renderLogResult logdisplay log (RD.Success ({ pkgname })) { ghcVer, pkgVer } =
  HH.div
    [ HP.id_ "tabs"
    , HP.classes (H.ClassName <$> ["ui-tabs","ui-widget","ui-widget-content","ui-corner-all"])
    , CSS.style $ D.display logdisplay
    ]
    [ HH.ul
        [ HP.classes (H.ClassName <$> ["ui-tabs-nav","ui-helper-reset","ui-helper-clearfix","ui-widget-header","ui-corner-all"])
        , HP.attr (H.AttrName "role") "tabs"
        ]
        [ HH.li
            [ HP.classes (H.ClassName <$> ["ui-state-default","ui-corner-top","ui-tabs-active","ui-state-active"])
            , HP.attr (H.AttrName "role") "tab"
            ]
            [ HH.a
                [ HP.class_ (H.ClassName "ui-tabs-anchor")
                , HP.attr (H.AttrName "role") "presentation"
                ]
                [ HH.text $ cellHash ghcVer pkgname pkgVer ]
            ]
        ]
    , HH.div
        [ HP.id_ "fragment-1"
        , HP.classes (H.ClassName <$> ["ui-tabs-panel","ui-widget-content","ui-corner-bottom"])
        , HP.attr (H.AttrName "role") "tabpanel"
        ]
        [ HH.pre
            [ HP.class_ (H.ClassName "log-entry") ]
            [ HH.text log ]
        ]
    ]
renderLogResult _ _ _ _ = HH.div_ []

renderPackageTag ::  forall p. T.PackageName
                 -> RD.RemoteData E.Error (Array T.TagName)
                 -> T.TagName
                 -> Array (HH.HTML p (Query Unit))
renderPackageTag pkgName (RD.Success tags) newtag =
  (\x -> HH.li_
           [ HH.span_ [HH.text $ x]
           , HH.a
               [ HP.class_ (H.ClassName "remove")
               , HE.onClick $ HE.input_ (RemoveTag x pkgName )
               ]
               [HH.text "╳"]
           ]
  ) <$> tags
renderPackageTag pkgName _ _ = []

generateIndexStateButton :: forall p. State -> Array (HH.HTML p (Query Unit))
generateIndexStateButton st =
  [ HH.div
      [ HP.class_ (H.ClassName "form") ]
        [ HH.label_
            [ HH.text "index-state: "
            , HH.select
                [ HP.class_ (H.ClassName "prio")
                , HE.onSelectedIndexChange $ HE.input (HandleIndex st)
                ] $ createIndexOption st <$> st.listTimeStamp
            ]
        ]
  ]

timeStampIsEmpty :: forall p i. Array (HH.HTML p i)
timeStampIsEmpty =
  [ HH.div
      [ HP.class_ (H.ClassName "success") ]
      [ HH.text "Index State is empty." ]
  ]

renderTableMatrix :: forall p. State
                  -> HH.HTML p (Query Unit)
renderTableMatrix {report: RD.Success (pkgreport), history} =
  HH.table_
    [ HH.thead_
        [ HH.tr_ $
            [ HH.th_
                [ HH.a
                    [ HP.href $ "https://hackage.haskell.org/package/" <> pkgreport.pkgname ]
                    [ HH.text pkgreport.pkgname ]
                ]
            ] <> ((\x -> HH.th_ $ [HH.text (ghcVersions x)]) <$> pkgreport.hcversions)
        ]
    , HH.tbody_ $ Arr.reverse $ getTheResult accumResult
    ]
  where
    accumResult = mapAccumL (generateTableRow pkgreport)
                            (Tuple.Tuple "" 0)
                            history
    getTheResult { value } = value
    ghcVersions x =
      case Str.stripPrefix (Str.Pattern "ghc-") x of
        Just a -> a
        Nothing -> "Failed"
renderTableMatrix _ =
  HH.table_
    [ HH.thead_
        [ HH.tr_ $
            [ HH.th_
                [ HH.a
                    [ HP.href $ "https://hackage.haskell.org/package/" ]
                    [ HH.text "" ]
                ]
            ]
        ]
    , HH.tbody_ []
    ]

generateTableRow :: forall p. T.PackageIdxTsReports
                 -> Tuple.Tuple T.VersionName T.Revision
                 -> Tuple.Tuple T.VersionName T.Revision
                 -> Accum (Tuple.Tuple T.VersionName T.Revision) (HH.HTML p (Query Unit))
generateTableRow pkgreport prevVer currentVer  =
  { accum: const currentVer prevVer
  , value:
      if Arr.null ghcResult
         then
          HH.tr
             [ HP.classes (H.ClassName <$> (["solver-row"] <> packageVersioning (minorCheck pv cv)
                                                                                (majorCheck pv cv))) ] $
             [ HH.th
                 [ HP.class_ (H.ClassName "pkgv") ] $
                 [ HH.a
                     [ HP.href $ hdiffUrl pkgreport.pkgname pv cv
                     ]
                     [ HH.text "Δ" ]
                 , HH.a
                     [ HP.href $ hackageUrl pkgreport.pkgname cv ]
                     [ HH.text cv ]
                 ] <> (if Tuple.snd currentVer > 0
                       then [ (containedRevision pkgreport.pkgname cv (Tuple.snd currentVer)) ]
                       else [])
             ] <> Arr.reverse (generateTableColumn pkgreport cv ghcResult <$> (Arr.reverse buildEmpty))
         else
          HH.tr
             [ HP.classes (H.ClassName <$> (["solver-row"] <> packageVersioning (minorCheck pv cv)
                                                                                (majorCheck pv cv))) ] $
             [ HH.th
                 [ HP.class_ (H.ClassName "pkgv") ] $
                 [ HH.a
                     [ HP.href $ hdiffUrl pkgreport.pkgname pv cv
                     ]
                     [ HH.text "Δ" ]
                 , HH.a
                     [ HP.href $ hackageUrl pkgreport.pkgname cv ]
                     [ HH.text cv ]
                 ] <> (if Tuple.snd currentVer > 0
                       then [ (containedRevision pkgreport.pkgname cv (Tuple.snd currentVer)) ]
                       else [])
             ] <> Arr.reverse (generateTableColumn pkgreport cv ghcResult <$> (Arr.reverse buildReport))
  }
  where
    ghcResult =
      case SM.lookup cv pkgreport.pkgversions of
        Just arr -> arr
        Nothing  -> []
    cv = Tuple.fst currentVer
    pv = Tuple.fst prevVer
    buildReport = Arr.zip pkgreport.hcversions ghcResult
    buildEmpty = (\x -> Tuple.Tuple x reportDefault) <$> pkgreport.hcversions
    reportDefault = {crsT: "na", crsBjle: 0, crsPerr: false, crsBok: 0, crsBfail: 0, crsBdfail: 0}
    splitPrevVer prev = if prev == "0" then [""] else splitVersion prev
    splitCurrVer curr = splitVersion curr
    majorCheck prev curr = newMajor (splitPrevVer prev) (splitCurrVer curr)
    minorCheck prev curr = newMinor (splitPrevVer prev) (splitCurrVer curr)

splitVersion :: T.VersionName -> Array T.VersionName
splitVersion v = Str.split (Str.Pattern ".") v

newMajor :: Array T.VersionName -> Array T.VersionName -> Boolean
newMajor a b
  | a == [""] =  true
  | a == b    =  false
  | otherwise =  (a Arr.!! 0) /= (b Arr.!! 0)
              || (fromMaybe "0" (a Arr.!! 1) /= fromMaybe "0" (b Arr.!! 1))

newMinor :: Array T.VersionName -> Array T.VersionName -> Boolean
newMinor a b
  | a == [""] =  true
  | a == b    =  false
  | otherwise =  newMajor a b
              || ((fromMaybe "0" (a Arr.!! 2)) /= (fromMaybe "0" (b Arr.!! 2)))

packageVersioning :: Boolean -> Boolean  -> Array String
packageVersioning minor major
  | minor && major = ["first-major", "first-minor"]
  | major          = ["first-major"]
  | minor          = ["first-minor"]
  | otherwise      = []

generateTableColumn :: forall p. T.PackageIdxTsReports
                    -> T.VersionName
                    -> Array T.CellReportSummary
                    -> Tuple.Tuple T.HCVer T.CellReportSummary
                    -> HH.HTML p (Query Unit)
generateTableColumn package verN ghcRes (Tuple.Tuple ghcV summ) =
  if Arr.null ghcRes then renderNotContained
  else
    case summ.crsT of
      "na" -> renderNotContained
      _    -> renderContained
  where
    renderContained =
        HH.td
          [ HP.classes $ H.ClassName <$>
                          (["stcell"] <>
                           (checkPassOrFail summ))
          , HP.attr (H.AttrName "data-ghc-version") ghcV
          , HP.attr (H.AttrName "data-package-version") verN
          , HE.onClick $ HE.input_ (HighlightCell package summ ghcVer verName)
          ] $ [ HH.text (checkShallow summ) ]
    renderNotContained =
        HH.td
          [ HP.classes (H.ClassName <$> (["stcell", "fail-unknown"]))
          , HP.attr (H.AttrName "data-ghc-version") ghcV
          , HP.attr (H.AttrName "data-package-version") verN
          , HE.onClick $ HE.input_ (HighlightCell package summ ghcVer verName)
          ] $ [ HH.text ""]
    ghcVer = if Str.null ghcV then Nothing else pure ghcV
    verName = if Str.null verN then Nothing else pure verN

containedRevision :: forall p i. T.PackageName
                  -> T.VersionName
                  -> T.Revision
                  -> HH.HTML p i
containedRevision pkgName verName revision =
    HH.sup_
      [ HH.a
          [ HP.class_ (H.ClassName "revision")
          , HP.href $ revisionsUrl pkgName verName
          , HP.attr (H.AttrName "data-revision") (show revision)
          , HP.attr (H.AttrName "data-version") verName
          ]
          [ HH.text $ "-r" <> (show revision) ]
      ]

checkPassOrFail :: T.CellReportSummary -> Array String
checkPassOrFail summ =
  case summ.crsT of
    "pf" -> ["pass-no-ip"]
    "se" -> getBuild summ
    _    -> ["fail-unknown"]
  where
    getBuild summ
      | summ.crsBjle == 1 = ["fail-bj"]
      | summ.crsBok == 1 = ["pass-build"]
      | summ.crsBfail == 1 = ["fail-build"]
      | summ.crsBdfail == 1 = ["fail-dep-build"]
      | otherwise = ["fail-unknown"]


checkShallow :: T.CellReportSummary -> String
checkShallow summ =
  case summ.crsT of
    "pf" -> "OK (no-ip)"
    "se" -> getBuild summ
    _    -> ""
  where
    getBuild summ
      | summ.crsBjle == 1 = "FAIL (BJ)"
      | summ.crsBok == 1 = "OK"
      | summ.crsBfail == 1 = "FAIL (pkg)"
      | summ.crsBdfail == 1 = "FAIL (deps)" 
      | otherwise = ""

buildText :: String -> String
buildText str =
  case str of
    "bok"    -> "OK"
    "bjle"   -> "FAIL (BJ)"
    "bfail"  -> "FAIL (PKG)"
    "bdfail" -> "FAIL (DEPS)"
    _        -> "FAIL Unknown"

hdiffUrl :: T.PackageName -> T.HCVer -> T.HCVer -> T.HdiffUrl
hdiffUrl pkgName prevVer currVer
  | prevVer == "0"             = "http://hdiff.luite.com/cgit/" <> pkgName <> "/commit?id=" <> currVer
  | currVer == prevVer         = "http://hdiff.luite.com/cgit/" <> pkgName <> "/diff?id=" <> currVer
  | otherwise = "http://hdiff.luite.com/cgit/" <>
                                         pkgName <> "/diff?id=" <>
                                         currVer <> "&id2="
                                         <> prevVer

hackageUrl :: T.PackageName -> T.VersionName -> T.HackageUrl
hackageUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/" <> pkgName <> ".cabal/edit"

revisionsUrl :: T.PackageName -> T.VersionName -> T.RevisionUrl
revisionsUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/revisions"


cellHash :: Maybe T.HCVer -> T.PackageName -> Maybe T.VersionName -> T.Cell
cellHash (Just ghcVer) pkgName (Just pkgVer) =
  Str.toUpper ghcVer <> "/" <> pkgName <> "-" <> pkgVer
cellHash  _ pkgName  _ = "- /" <> pkgName <> "/ -"

getIdx :: T.PackageTS -> Array T.PkgIdxTs -> T.PkgIdxTs
getIdx idx listIdx =
  if Str.null idx
  then Misc.getLastIdx listIdx
  else
    case Foldable.find (\x -> idx == show x) listIdx of
      Just a -> a
      Nothing -> 0

renderUnitsButton :: forall p. State -> Array (SM.StrMap T.BuildStatus) -> T.ColumnVersion -> Array (HH.HTML p (Query Unit))
renderUnitsButton st arrUnit {ghcVer: (Just ghc), pkgVer: (Just pkg)} =
  let
    keys = Arr.concat (SM.keys <$> arrUnit)
    values = Arr.concat (SM.values <$> arrUnit)
    buttonRef :: Array (Tuple.Tuple String T.BuildStatus)
    buttonRef = Arr.zip keys values
  in
    [
      HH.div
        [HP.id_ "menuIdx"
        , HP.class_ (H.ClassName "idxOuter")
        ] $
        [ HH.h4
            [ HP.class_ (H.ClassName "logs-header") ]
            [ HH.text $ "There is at least one install-plan for " <> pkg <> " with " <> ghc <> " : "]
        ] <> ((renderButton ghc pkg) <$> buttonRef)
    ]
renderUnitsButton _ _ _ = []

renderButton :: forall p. T.HCVer -> T.VersionName -> (Tuple.Tuple String T.BuildStatus) -> HH.HTML p (Query Unit)
renderButton ghcVer pkgVer (Tuple.Tuple units status) =
  HH.button
      [ HP.class_ (H.ClassName "idxBtn")
      , HP.title ("Button for unit : " <> units)
      , HE.onClick $ HE.input_ (HighlightSE (Tuple.Tuple units status))
      ]
      [ HH.text (buildText status) ]



renderNavBtn :: forall p. State -> Array (HH.HTML p (Query Unit))
renderNavBtn st =
  [
    HH.div
      [ HP.id_ "menuIdx"
      , HP.class_ (H.ClassName "idxOuter")
      ] $
      [ HH.button
          [ HP.class_ (H.ClassName "idxBtn")
          , HP.title "First Index-State"
          , HE.onClick $ HE.input_ (HandleIndex st (case Map.findMin st.mapIndexState of
                                                       Just { key } -> (key :: Int)
                                                       Nothing      -> 0
                                                   ))
          ]
          [ HH.text "|< First" ]
      , HH.button
          [ HP.class_ (H.ClassName "idxBtn")
          , HP.title "Previous Index-State"
          , HE.onClick $ HE.input_ (HandleIndex st (if st.currKey == 0 then st.currKey else st.currKey - 1))
          ]
          [ HH.text "< Previous" ]
      ] <> (if Arr.null st.listTimeStamp then timeStampIsEmpty else generateIndexStateButton st) <> [ HH.button
                     [ HP.class_ (H.ClassName "idxBtn")
                     , HP.title "Next Index-State"
                     , HE.onClick $ HE.input_ (HandleIndex st (st.currKey + 1))
                     ]
                     [ HH.text "Next >" ]
                 , HH.button
                     [ HP.class_ (H.ClassName "idxBtn")
                     , HP.title "Last Index-State"
                     , HE.onClick $ HE.input_ (HandleIndex st (case Map.findMax st.mapIndexState of
                                                                  Just { key } -> (key :: Int)
                                                                  Nothing      -> 0
                                                              ))
                     ]
                     [ HH.text "Last >|" ]
                 ]
  ]

legend :: forall p. HH.HTML p (Query Unit)
legend =
  HH.div
    [ HP.class_ (H.ClassName "sub") ]
        [ HH.h4
            [ HP.id_ "legend"
            , HP.class_ (H.ClassName "header") ]
            [ HH.text "Legend" ]
        , HH.table
            [ HP.id_ "legend-table" ]
            [ HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["pass-build", "stcell"]) ]
                    [ HH.text "OK" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "package build succesful" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["pass-no-op", "stcell"]) ]
                    [ HH.text "OK (boot)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "pre-installed version" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["pass-no-ip", "stcell"]) ]
                    [ HH.text "OK (no-ip)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "no install-plan found" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["fail-bj", "stcell"]) ]
                    [ HH.text "FAIL (BJ)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "backjump limit reached" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["fail-build", "stcell"]) ]
                    [ HH.text "FAIL (pkg)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "package failed to build" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["fail-dep-build", "stcell"]) ]
                    [ HH.text "FAIL (deps)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "package dependencies failed to build" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["fail-no-ip", "stcell"]) ]
                    [ HH.text "FAIL (no-ip)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "something went horribly wrong" ]
                ]
            , HH.tr_
                [ HH.td
                    [ HP.classes (H.ClassName <$> ["fail-unknown", "stcell"]) ]
                    [ HH.text "FAIL (unknown)" ]
                , HH.td
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "test-result missing" ]
                ]
            ]
        , HH.button
            [ HP.class_ (H.ClassName "refresh")
            , HP.title "Refresh listings"
            , HE.onClick $ HE.input_ (Initialize)
            ]
            [ HH.text "Refresh listings" ]
        ]

sortVer :: Tuple.Tuple T.VersionName T.Revision -> Tuple.Tuple T.VersionName T.Revision -> Ordering
sortVer (Tuple.Tuple ver1 _) (Tuple.Tuple ver2 _) =
  let
    v1 = ((fromMaybe 0) <<< Int.fromString) <$> Str.split (Str.Pattern ".") ver1
    v2 = ((fromMaybe 0) <<< Int.fromString) <$> Str.split (Str.Pattern ".") ver2
  in
   compare v1 v2

