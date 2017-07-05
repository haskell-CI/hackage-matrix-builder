module Components.PagePackage where

import CSS.Display as D
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.Types as T
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Traversable (Accum, mapAccumL)
import Prelude (type (~>), Unit, Void, bind, const, discard, map, otherwise, pure, show, ($), (&&), (/=), (<$>), (<>), (==), (>), (||), (<<<))

type State =
  { initPackage :: T.PackageName
  , logdisplay  :: D.Display
  , package     :: T.Package
  , report      :: T.ShallowReport
  , highlighted :: Boolean
  , logmessage  :: String
  , columnversion :: T.ColumnVersion
  , selectedpackage :: T.PackageMeta
  , newtag :: T.TagName
  }

data Query a
  = Initialize a
  | QueueingPackage a
  | FailingPackage a
  | HighlightCell T.PackageName T.VersionName T.VersionName a
  | HandleTag T.TagName a
  | AddingNewTag T.PackageName T.TagName a
  | QueueBuild a
  | Finalize a


component :: forall e. H.Component HH.HTML Query String Void (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , initializer: Just (H.action Initialize) 
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

    initialState :: String -> State
    initialState i =
      {
        initPackage: i
      , logdisplay: D.displayNone
      , package: { name: ""
                 , versions: []
                 }
      , report: { packageName: ""
                , modified: ""
                , results: []
                }
      , highlighted: false
      , logmessage: ""
      , columnversion: { ghcVer: ""
                       , pkgVer: ""
                       }
      , selectedpackage: { name: ""
                         , report: Nothing
                         , tags: []
                         }
      , newtag: ""
      }

    render :: State -> H.ComponentHTML Query
    render state =
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
                [ HH.text state.package.name ]
            , HH.div
                [ HP.classes (H.ClassName <$> ["main-header-subtext","last-build"]) ]
                [ HH.text $ "index-state: " -- <> fromDate args will be based on package meta that get passed from page-packages
                ]
            , HH.div
                [ HP.id_ "package-buildreport" ]
                [ HH.h3
                    [ HP.class_ (H.ClassName "package-header") ]
                    [ HH.text "Solver Matrix (constrained by single version) " ]
                , HH.div
                    [ HP.id_ "package" ] [ (renderTableMatrix state.package state.report) ]
                , HH.h3
                    [ HP.class_ (H.ClassName "logs-header") ]
                    [ HH.text "Logs" ]
                , HH.div
                    [ HP.id_ "tabs-container" ]
                    [ (renderLogResult state.logdisplay state.logmessage state.package state.columnversion) ]
                ]
            ]
        ]
      where
        tagging {package, newtag, selectedpackage} =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Tags" ]
            , HH.div
                [ HP.id_ "tagging" ]
                [ HH.ul
                    [ HP.class_ (H.ClassName "tags") ] $
                    renderPackageTag selectedpackage -- TODO : This is where the list of tags generated based on the selected package
                , HH.div
                    [ HP.class_ (H.ClassName "form") ]
                    [ HH.label_
                        [ HH.text "Tag"
                        , HH.input
                            [ HP.type_ HP.InputText
                            , HP.class_ (H.ClassName "tag-name")
                            , HE.onValueInput (HE.input HandleTag)
                            -- TODO : save the text.
                            ]

                        ]
                    , HH.button
                        [ HP.class_ (H.ClassName "action")
                        , HE.onClick $ HE.input_ (AddingNewTag package.name newtag)
                        -- TODO : When this button clicked, it will get text then add tag to the <ul class="tags"> above
                        ]
                        [ HH.text "Add Tag" ]
                     ]
                ]
            ]
        queueing {package, newtag} =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Queueing" ]
            , HH.div
                [ HP.id_ "queueing" ]
                [ HH.div
                    [ HP.class_ (H.ClassName "form") ]
                    [ HH.label_
                        [ HH.text "Priority"
                        , HH.select
                            [ HP.class_ (H.ClassName "prio")
                            -- , HE.onSelectedIndexChange $ HE.input QueueingPackage
                            ]
                            [ HH.option
                                [ HP.value "high" ]
                                [ HH.text "High" ]
                            , HH.option
                                [ HP.value "medium"
                                , HP.selected true
                                ]
                                [ HH.text "medium" ]
                            , HH.option
                                [ HP.value "low" ]
                                [ HH.text "Low" ]
                            ]
                        ]
                    , HH.button
                        [ HP.class_ (H.ClassName "action")
                        , HE.onClick $ HE.input_ QueueBuild
                        ]
                        [ HH.text "Queue build for this package" ]
                    ]
                ]
            ]

    eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
    eval (Initialize next) = do
      st <- H.get
      packageByName <- H.lift $ Api.getPackageByName st.initPackage
      reportPackage <- H.lift $ Api.getLatestReportByPackageName st.initPackage
      initState <- H.put $ st { package = packageByName, report = reportPackage, highlighted = false}
      pure next
    eval (FailingPackage next) = do
      pure next
    eval (HighlightCell pkgName ghcVer pkgVer next) = do
      singleResult <- H.lift $ Api.getSingleResult pkgName (ghcVer <> "-" <> pkgVer)
      H.modify \st -> st { logmessage = (if ( isContainedLog singleResult.resultA ) then "" else pickLogMessage singleResult )
                         , logdisplay = D.block
                         , columnversion = { ghcVer, pkgVer }
                         }
      pure next
    eval (HandleTag value next) = do
      st <- H.get
      H.put $ st { newtag = value }
      pure next
    eval (AddingNewTag pkgName newTag next) = do
      _ <- H.lift $ Api.putTagSaveByName pkgName newTag
      pure next
    eval (QueueingPackage next) = do
      pure next
    eval (QueueBuild next) = do
      pure next
    eval (Finalize next) = do
      pure next

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

renderPackageTag ::  forall p. T.PackageMeta -> Array (HH.HTML p (Query Unit))
renderPackageTag { tags } =
  (\x -> HH.li_
           [ HH.span_ [HH.text $ x]
           , HH.a
               [HP.class_ (H.ClassName "remove")]
               [HH.text "╳"]
           ]
  ) <$> tags

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
    accumResult = mapAccumL (generateTableRow package shallowR) (Arr.head (package.versions)) package.versions
    getTheResult { value } = value

ghcVersions :: Array T.VersionName
ghcVersions = ["8.2","8.0","7.10","7.8","7.6","7.4"]

generateTableRow :: forall p. T.Package
                 -> T.ShallowReport
                 -> Maybe T.VersionInfo
                 -> T.VersionInfo
                 -> Accum (Maybe T.VersionInfo) (HH.HTML p (Query Unit))
generateTableRow package { results } Nothing currentVer =
   { accum: Nothing
   , value: HH.tr_ []
   }
generateTableRow package { results } (Just prevVer) currentVer  =
  { accum: const (Just currentVer) prevVer
  , value: HH.tr
             [ HP.classes (H.ClassName <$> (["solver-row"] <> packageVersioning majorCheck minorCheck)) ] $
             [ HH.th
                 [ HP.class_ (H.ClassName "pkgv") ] $
                 [ HH.a
                     [ HP.href $ hdiffUrl package.name prevVer currentVer
                     ]
                     [ HH.text "Δ" ]
                 , HH.a
                     [ HP.href $ hackageUrl package.name currentVer.version ]
                     [ HH.text currentVer.version ]
                 ] <> (if currentVer.revision > 0 then [ (containedRevision package.name currentVer.version currentVer.revision) ]
                         else [])
             ] <> Arr.reverse (generateTableColumn package currentVer.version <$> results)
  }
  where
    splitPrevVer = maybe [] splitVersion (Just prevVer.version)
    splitCurrVer = splitVersion currentVer.version
    majorCheck = newMajor splitPrevVer splitCurrVer
    minorCheck = newMinor splitPrevVer splitCurrVer



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

generateTableColumn :: forall p. T.Package
                    -> T.VersionName
                    -> T.ShallowGhcResult
                    -> HH.HTML p (Query Unit)
generateTableColumn package verName { ghcVersion, ghcResult } =
    HH.td
      [ HP.classes (H.ClassName <$> (["stcell"] <> (Arr.concat $ checkPassOrFail verName <$> ghcResult)))
      , HP.attr (H.AttrName "data-ghc-version") ghcVersion
      , HP.attr (H.AttrName "data-package-version") verName
      , HE.onClick $ HE.input_ (HighlightCell package.name ghcVersion verName)
      ] $
      [ HH.text (Str.joinWith "" $ checkShallow verName <$> ghcResult) ]

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
  | currVer.version == prevVer.version = "http://hdiff.luite.com/cgit/" <> pkgName <> "/diff?id=" <> currVer.version
  | currVer.version /= prevVer.version = "http://hdiff.luite.com/cgit/" <>
                                         pkgName <> "/diff?id=" <>
                                         currVer.version <> "&id2="
                                         <> prevVer.version
  | otherwise                          = "http://hdiff.luite.com/cgit/" <> pkgName <> "/commit?id=" <> currVer.version

hackageUrl :: T.PackageName -> T.VersionName -> T.HackageUrl
hackageUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/" <> pkgName <> ".cabal/edit"

revisionsUrl :: T.PackageName -> T.VersionName -> T.RevisionUrl
revisionsUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/revisions"


cellHash :: T.VersionName -> T.PackageName -> T.VersionName -> T.Cell
cellHash ghcVer pkgName pkgVer =
  "GHC-" <> ghcVer <> "/" <> pkgName <> "-" <> pkgVer

legend :: forall p i. HH.HTML p i
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
        ]
