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
import Lib.MatrixApi2 as Api
import Lib.Types as T
import Network.RemoteData as RD
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Traversable (Accum, mapAccumL)
import Lib.MiscFFI as Misc
import Prelude (type (~>), Unit, bind, const, discard, otherwise, pure, show, ($), (&&), (/=), (<$>), (<>), (==), (>), (||), (>>=), (-), (*))
import DOM.HTML (window) as DOM
import DOM.HTML.History (DocumentTitle(DocumentTitle), URL(URL), pushState) as DOM
import DOM.HTML.Window (history) as DOM
import Debug.Trace
import Data.Tuple as Tuple
import Data.Argonaut as Arg
import Data.Foreign as F
import Data.StrMap as SM
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Formatter.DateTime as FDT
import Data.Time.Duration (Milliseconds(Milliseconds)) as DT
import Data.DateTime (DateTime) as DT
import Data.DateTime.Instant (instant, toDateTime) as DT

type State =
  { initPackage :: Tuple.Tuple T.PackageName T.PackageTS
  , logdisplay  :: D.Display
  , package     :: T.Package
  , report      :: RD.RemoteData E.Error T.ShallowReport
  , highlighted :: Boolean
  , logmessage  :: String
  , columnversion :: T.ColumnVersion
  , newtag :: T.TagName
  , newPrio :: T.Priority
  , queueStatus :: RD.RemoteData E.Error (Maybe T.QueueItem)
  , queueClicked :: Boolean
  , listTimeStamp :: Array T.PkgIdxTs
  , currentSelectedIdx :: T.PackageTS
  , latestIndex :: T.PackageTS
  , mapIndexState :: Map.Map Int T.PackageTS
  , listTags :: Array T.TagName
  }

data Query a
  = Initialize a
  | FailingPackage a
  | HighlightCell T.PackageName T.VersionName T.VersionName a
  | HandleTag T.TagName a
  | Receive (Tuple.Tuple T.PackageName T.PackageTS) a
  | AddingNewTag T.PackageName T.TagName a
  | RemoveTag T.PackageName T.TagName a
  | UpdateTag (T.PackageName -> a)
  | HandleQueue Int a
  | QueueBuild T.PackageName T.Priority (RD.RemoteData E.Error (Maybe T.QueueItem)) a
  | HandleIndex State Int a
  | Finalize a

data Message = FromPagePackage

ghcVersions :: Array T.VersionName
ghcVersions = ["8.2","8.0","7.10","7.8","7.6","7.4"]

component :: forall e.
             H.Component HH.HTML Query (Tuple.Tuple T.PackageName T.PackageTS) Message (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: HE.input Receive
  }
  where

    initialState :: Tuple.Tuple T.PackageName T.PackageTS -> State
    initialState i =
      {
        initPackage: i
      , logdisplay: D.displayNone
      , package: { name: ""
                 , versions: []
                 }
      , report: RD.NotAsked
      , highlighted: false
      , logmessage: ""
      , columnversion: { ghcVer: ""
                       , pkgVer: ""
                       }
      , newtag: ""
      , newPrio: T.Low
      , queueStatus: RD.NotAsked
      , queueClicked: false
      , listTimeStamp: []
      , currentSelectedIdx: ""
      , latestIndex: ""
      , mapIndexState: Map.empty
      , listTags: []
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
                [ HP.class_ (H.ClassName "leftcol") ]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text $ _.name state.package ]
                , HH.div
                    [ HP.classes (H.ClassName <$> ["main-header-subtext","last-build"]) ]
                    [ HH.text $ "index-state: " <> (Misc.formatDate' state.report)                ]
                , HH.div
                    [ HP.id_ "package-buildreport" ]
                    [ HH.h3
                        [ HP.class_ (H.ClassName "package-header") ]
                        [ HH.text "Solver Matrix (constrained by single version) " ]
                    , HH.div
                        [ HP.id_ "package" ] [ (renderTableMatrix state.package shallowR) ]
                    , HH.h3
                        [ HP.class_ (H.ClassName "logs-header") ]
                        [ HH.text "Logs" ]
                    , HH.div
                        [ HP.id_ "tabs-container" ]
                        [ (renderLogResult state.logdisplay state.logmessage state.package state.columnversion) ]
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
                    [ HH.text $ _.name state.package ]
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
                    [ HH.text $ _.name state.package ]
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
                        [ HP.id_ "package" ]
                        [ (renderTableMatrix state.package { packageName: _.name state.package, modified: "", results: [] }) ]
                    , HH.h3
                        [ HP.class_ (H.ClassName "logs-header") ]
                        [ HH.text "Logs" ]
                    , HH.div
                        [ HP.id_ "tabs-container" ]
                        [ (renderLogResult state.logdisplay state.logmessage state.package state.columnversion) ]
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
                    renderPackageTag (Tuple.fst st.initPackage) st.listTags st.newtag
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
                                                               (Tuple.fst st.initPackage))
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
      Tuple.Tuple _ idx' <-  Api.latestIndex (Tuple.fst st.initPackage)
      pkgTs <- Api.getPackageReports  (Tuple.fst st.initPackage)
      tags <- Api.getPackageTags (Tuple.fst st.initPackage) >>= Api.parseJsonToArrayS
      let
        listIndex =
          case pkgTs of
            RD.Success idx' -> Int.round <$> (Misc.fromIndexToNumber (Arg.toArray idx'))
            _              -> []
        latestIdx = Misc.getLastIdx listIndex
        mapIdxSt = Map.fromFoldable (toTupleArray listIndex)
      packageByName <- H.lift $ Api.getPackageByName (Tuple.fst st.initPackage)
      reportPackage <- H.lift $
        if Str.null (Tuple.snd st.initPackage)
        then Api.getLatestReportByPackageName (Tuple.fst st.initPackage)
        else (Api.getLatestReportByPackageTimestamp (Tuple.fst st.initPackage) (Tuple.snd st.initPackage))
                >>= Api.parseShallowReport
      queueStat <- H.lift $ Api.getQueueByName (Tuple.fst st.initPackage)
      H.modify _  { package = packageByName
                  , report = reportPackage
                  , highlighted = false
                  , queueStatus = queueStat
                  , listTimeStamp = listIndex
                  , currentSelectedIdx = Tuple.snd st.initPackage
                  , latestIndex = latestIdx
                  , mapIndexState = mapIdxSt
                  , listTags =
                      case tags of
                        (RD.Success a) -> a
                        _              -> []
                  }
      pure next
    eval (FailingPackage next) = do
      pure next
    eval (HighlightCell pkgName ghcVer pkgVer next) = do
      singleResult <- H.lift $ Api.getSingleResult pkgName (ghcVer <> "-" <> pkgVer)
      H.modify _ { logmessage = (if ( isContainedLog singleResult.resultA )
                                 then ""
                                 else pickLogMessage singleResult )
                 , logdisplay = D.block
                 , columnversion = { ghcVer, pkgVer }
                 }
      pure next
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
      Tuple.Tuple _ idx' <- Api.latestIndex (Tuple.fst pkg)
      pkgTs <- Api.getPackageReports (Tuple.fst pkg)
      tags <- Api.getPackageTags (Tuple.fst pkg) >>= Api.parseJsonToArrayS
      let
        listIndex =
          case pkgTs of
            RD.Success idx' -> Int.round <$> (Misc.fromIndexToNumber (Arg.toArray idx'))
            _              -> []
        latestIdx = Misc.getLastIdx listIndex
        mapIdxSt = Map.fromFoldable (toTupleArray listIndex)
      packageByName <- H.lift $ Api.getPackageByName (Tuple.fst pkg)
      reportPackage <- H.lift $
        if Str.null (Tuple.snd pkg)
        then  Api.getLatestReportByPackageName (Tuple.fst pkg)
        else  (Api.getLatestReportByPackageTimestamp (Tuple.fst pkg) (Tuple.snd pkg))
                 >>= Api.parseShallowReport
      queueStat <- H.lift $ Api.getQueueByName (Tuple.fst pkg)
      H.modify _ { initPackage = pkg
                 , package = packageByName
                 , report = reportPackage
                 , queueStatus = queueStat
                 , listTimeStamp = listIndex
                 , latestIndex = latestIdx
                 , currentSelectedIdx = Tuple.snd pkg
                 , mapIndexState = mapIdxSt
                 , listTags =
                      case tags of
                        (RD.Success a) -> a
                        _              -> []
                 }
      pure next
    eval (HandleQueue idx next) = do
      _ <- case idx of
              1 -> H.modify _ { newPrio = T.Medium}
              2 -> H.modify _ { newPrio = T.Low}
              _ -> H.modify _ { newPrio = T.High}
      pure next
    eval (QueueBuild pkgName prio (RD.Success _) next ) = do
      _ <- H.lift $ Api.putQueueSaveByName pkgName prio
      H.modify _ { queueClicked = true }
      pure next
    eval (QueueBuild pkgName prio _ next) = do
      H.modify _ { queueClicked = true }
      _ <- H.lift $ Api.putQueueCreate pkgName prio
      pure next
    eval (HandleIndex st idx next) = do
      traceAnyA ("current index is : " <> (show idx))
      let
        selectedIndex' =
          case Map.lookup idx st.mapIndexState of
            Just a -> a
            Nothing -> ""
        package = Tuple.fst st.initPackage
        sObj = SM.singleton "name" (Arg.encodeJson (_.name st.package))
        jObj = F.toForeign (SM.insert "index" (Arg.encodeJson selectedIndex') sObj)
        indexURL = if Str.null selectedIndex' then "" else "@" <> selectedIndex'
        pageName = DOM.DocumentTitle $ (_.name st.package) <> " - " <> selectedIndex'
        pageUrl = DOM.URL $ "#/package/" <> (_.name st.package) <> indexURL
      hist <- H.liftEff $ DOM.window >>= DOM.history
      pushS <- H.liftEff $ DOM.pushState jObj pageName pageUrl hist
      traceAnyA selectedIndex'
      eval (Receive (Tuple.Tuple package selectedIndex') next)
    eval (Finalize next) = do
      pure next

checkQueueStatus :: forall p i. RD.RemoteData E.Error (Maybe T.QueueItem) ->  Array (HH.HTML p i)
checkQueueStatus (RD.Success (Just qi)) =
  [ HH.div
      [ HP.class_ (H.ClassName "already-queued") ]
      [ HH.text ("This package is already in " <>
                 qi.priority <>
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
                    ([ HP.value "high" ] <> isPrioSelected "high" st.queueStatus)
                    [ HH.text "High" ]
                , HH.option
                    ([ HP.value "medium" ] <> isPrioSelected "medium" st.queueStatus)
                    [ HH.text "Medium" ]
                , HH.option
                    ([ HP.value "low" ] <> isPrioSelected "low" st.queueStatus)
                    [ HH.text "Low" ]
                ]
            ]
        , HH.button
            [ HP.class_ (H.ClassName "action")
            , HE.onClick $ HE.input_ (QueueBuild (Tuple.fst st.initPackage) st.newPrio st.queueStatus)
            ]
            [ HH.text "Queue build for this package" ]
        ]
  ]

generateIndexStateButton :: forall p. State -> Array (HH.HTML p (Query Unit))
generateIndexStateButton st =
  [ HH.div
      [ HP.class_ (H.ClassName "form") ]
        [ HH.label_
            [ HH.text "Index State"
            , HH.select
                [ HP.class_ (H.ClassName "prio")
                , HE.onSelectedIndexChange $ HE.input (HandleIndex st)
                ] $ createIndexOption st <$> st.listTimeStamp
            ]
        ]
  ]

toTupleArray :: Array T.PkgIdxTs -> Array (Tuple.Tuple Int T.PackageTS)
toTupleArray xs =
  let len = (Arr.length xs) - 1
      idx = Arr.(..) 0 len
  in Arr.zip idx (show <$> xs)

createIndexOption :: forall p i. State -> T.PkgIdxTs -> HH.HTML p i
createIndexOption st idx' =
  let
    idx = show idx'
  in
     HH.option
       ([ HP.value idx ] <> isIndexSelected st idx )
       [ HH.text (toDateTime idx') ]

toDateTime :: T.PkgIdxTs -> T.PackageTS
toDateTime idx =
  case ptimeToDateTime idx of
    (Just ts) ->
      case FDT.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" ts of
        (Right date) -> date
        Left _      -> ""
    Nothing   -> ""

ptimeToDateTime :: T.PkgIdxTs -> Maybe DT.DateTime
ptimeToDateTime idx = DT.toDateTime <$> DT.instant (DT.Milliseconds (1000.0 * (Int.toNumber idx)))

isIndexSelected :: forall p i. State
                -> T.PackageTS
                -> Array (HH.IProp (selected :: Boolean | p) i)
isIndexSelected st idx =
  case st.currentSelectedIdx == idx of
    true  -> [HP.selected true]
    false -> if Str.null st.currentSelectedIdx && st.latestIndex == idx
                then [HP.selected true]
                else []

isPrioSelected :: forall p i. String
               -> RD.RemoteData E.Error (Maybe T.QueueItem)
               -> Array (HH.IProp (selected :: Boolean | p) i)
isPrioSelected prio (RD.Success (Just qi)) =
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

renderLogResult :: forall p i. D.Display -> String -> T.Package -> T.ColumnVersion -> HH.HTML p i
renderLogResult logdisplay log { name } { ghcVer, pkgVer } =
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
                [ HH.text $ cellHash ghcVer name pkgVer ]
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

renderPackageTag ::  forall p. T.PackageName
                 -> Array T.TagName
                 -> T.TagName
                 -> Array (HH.HTML p (Query Unit))
renderPackageTag pkgName tags newtag =
  (\x -> HH.li_
           [ HH.span_ [HH.text $ x]
           , HH.a
               [ HP.class_ (H.ClassName "remove")
               , HE.onClick $ HE.input_ (RemoveTag x pkgName )
               ]
               [HH.text "╳"]
           ]
  ) <$> tags

timeStampIsEmpty :: forall p i. Array (HH.HTML p i)
timeStampIsEmpty =
  [ HH.div
      [ HP.class_ (H.ClassName "success") ]
      [ HH.text "Index State is empty." ]
  ]

pickLogMessage :: T.SingleResult -> String
pickLogMessage { resultA } = logMessage resultA

logMessage :: Maybe T.VersionResult -> String
logMessage (Just { result }) =
  case result of
    (T.Fail str) -> str
    _          -> " "
logMessage  _  = " "

isContainedLog :: Maybe T.VersionResult -> Boolean
isContainedLog versionR = Str.null $ logMessage versionR

renderTableMatrix :: forall p. T.Package
                  -> T.ShallowReport
                  -> HH.HTML p (Query Unit)
renderTableMatrix package shallowR =
  HH.table_
    [ HH.thead_
        [ HH.tr_ $
            [ HH.th_
                [ HH.a
                    [ HP.href $ "https://hackage.haskell.org/package/" <> package.name ]
                    [ HH.text package.name ]
                ]
            ] <> ((\x -> HH.th_ $ [HH.text x]) <$> ghcVersions)
        ]
    , HH.tbody_ $ Arr.reverse $ getTheResult accumResult
    ]
  where
    accumResult = mapAccumL (generateTableRow package shallowR)
                            ({version: "0", revision: 0, preference: T.Normal})
                            package.versions
    getTheResult { value } = value

generateTableRow :: forall p. T.Package
                 -> T.ShallowReport
                 -> T.VersionInfo
                 -> T.VersionInfo
                 -> Accum T.VersionInfo (HH.HTML p (Query Unit))
generateTableRow package shallowR prevVer currentVer  =
  { accum: const currentVer prevVer
  , value: HH.tr
             [ HP.classes (H.ClassName <$> (["solver-row"] <> packageVersioning (minorCheck prevVer currentVer)
                                                                                (majorCheck prevVer currentVer))) ] $
             [ HH.th
                 [ HP.class_ (H.ClassName "pkgv") ] $
                 [ HH.a
                     [ HP.href $ hdiffUrl package.name prevVer currentVer
                     ]
                     [ HH.text "Δ" ]
                 , HH.a
                     [ HP.href $ hackageUrl package.name currentVer.version ]
                     [ HH.text currentVer.version ]
                 ] <> (if currentVer.revision > 0
                       then [ (containedRevision package.name currentVer.version currentVer.revision) ]
                       else [])
             ] <> Arr.reverse (generateTableColumn package shallowR.results currentVer.version <$> (Arr.reverse ghcVersions))
  }
  where
    splitPrevVer prev = if prev.version == "0" then [""] else splitVersion prev.version
    splitCurrVer curr = splitVersion curr.version
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

generateTableColumn :: forall p. T.Package
                    -> Array T.ShallowGhcResult
                    -> T.VersionName
                    -> T.VersionName
                    -> HH.HTML p (Query Unit)
generateTableColumn package shallowGhcArr verName ghcVer =
  case isContainedVersion ghcVer verName shallowGhcArr && isContainedGHC ghcVer shallowGhcArr of
    true -> renderContained
    false -> renderNotContained
  where
    renderContained =
      HH.td
        [ HP.classes $ H.ClassName <$>
                        (["stcell"] <>
                         (Arr.concat $ checkPassOrFail verName <$>
                          (getShallowVer ghcVer shallowGhcArr)))
        , HP.attr (H.AttrName "data-ghc-version") ghcVer
        , HP.attr (H.AttrName "data-package-version") verName
        , HE.onClick $ HE.input_ (HighlightCell package.name ghcVer verName)
        ] $ [ HH.text (Str.joinWith "" $ checkShallow verName <$>
                                         (getShallowVer ghcVer shallowGhcArr)) ]
    renderNotContained =
      HH.td
        [ HP.classes (H.ClassName <$> (["stcell", "fail-unknown"]))
        , HP.attr (H.AttrName "data-ghc-version") ghcVer
        , HP.attr (H.AttrName "data-package-version") verName
        , HE.onClick $ HE.input_ (HighlightCell package.name ghcVer verName)
        ] $ [ HH.text ""]

getShallowVer :: T.VersionName -> Array T.ShallowGhcResult -> Array T.ShallowVersionResult
getShallowVer ghcVer ghcArr =
  case Arr.uncons filteredArr of
    Just { head: x, tail: xs } -> x.ghcResult
    Nothing                    -> []
  where
    filteredArr = Arr.filter (\x -> x.ghcVersion == ghcVer) ghcArr

containedRevision :: forall p i. T.PackageName
                  -> T.VersionName
                  -> T.Word
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

isContainedGHC ::  T.VersionName -> Array T.ShallowGhcResult -> Boolean
isContainedGHC ghcVer shallowGhcArr  = Arr.elem ghcVer (_.ghcVersion <$> shallowGhcArr)

isContainedVersion :: T.VersionName -> T.VersionName -> Array T.ShallowGhcResult -> Boolean
isContainedVersion ghcVer verName shallowGhcArr =
  let currGhcVer = Arr.head $ Arr.filter (\x -> ghcVer == x.ghcVersion) shallowGhcArr
  in
   case currGhcVer of
    (Just { ghcResult }) -> Arr.elem verName (_.packageVersion <$> ghcResult)
    Nothing              -> false

checkPassOrFail :: T.VersionName -> T.ShallowVersionResult -> Array String
checkPassOrFail verName { packageVersion, result }
  | verName == packageVersion = passOrFail result
  | otherwise                 = []

passOrFail :: T.ShallowResult -> Array String
passOrFail sR =
  case sR of
    T.ShallowOk            -> ["pass-build"]
    T.ShallowNop           -> ["pass-no-op"]
    T.ShallowNoIp          -> ["pass-no-ip"]
    T.ShallowNoIpBjLimit _ -> ["fail-bj"]
    T.ShallowNoIpFail      -> ["fail-no-ip"]
    T.ShallowFail          -> ["fail-build"]
    T.ShallowFailDeps _    -> ["fail-dep-build"]
    T.Unknown              -> ["fail-unknown"]

checkShallow :: T.VersionName -> T.ShallowVersionResult -> String
checkShallow verName { packageVersion, result }
  | verName == packageVersion = checkShallowResult result
  | otherwise                 = ""

checkShallowResult :: T.ShallowResult -> String
checkShallowResult sR =
  case sR of
    T.ShallowOk            -> "OK"
    T.ShallowNop           -> "OK (boot)"
    T.ShallowNoIp          -> "OK (no-ip)"
    T.ShallowNoIpBjLimit _ -> "FAIL (BJ)"
    T.ShallowNoIpFail      -> "FAIL (no-ip)"
    T.ShallowFail          -> "FAIL (pkg)"
    T.ShallowFailDeps _    -> "FAIL (deps)"
    T.Unknown              -> ""

hdiffUrl :: T.PackageName -> T.VersionInfo -> T.VersionInfo -> T.HdiffUrl
hdiffUrl pkgName prevVer currVer
  | prevVer.version == "0"             = "http://hdiff.luite.com/cgit/" <> pkgName <> "/commit?id=" <> currVer.version
  | currVer.version == prevVer.version = "http://hdiff.luite.com/cgit/" <> pkgName <> "/diff?id=" <> currVer.version
  | otherwise = "http://hdiff.luite.com/cgit/" <>
                                         pkgName <> "/diff?id=" <>
                                         currVer.version <> "&id2="
                                         <> prevVer.version

hackageUrl :: T.PackageName -> T.VersionName -> T.HackageUrl
hackageUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/" <> pkgName <> ".cabal/edit"

revisionsUrl :: T.PackageName -> T.VersionName -> T.RevisionUrl
revisionsUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/revisions"


cellHash :: T.VersionName -> T.PackageName -> T.VersionName -> T.Cell
cellHash ghcVer pkgName pkgVer =
  "GHC-" <> ghcVer <> "/" <> pkgName <> "-" <> pkgVer

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
