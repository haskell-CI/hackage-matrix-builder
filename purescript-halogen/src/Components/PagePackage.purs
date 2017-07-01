module Components.PagePackage where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Semigroup ((<>))
import Data.String as Str
import Data.Array (concat, reverse, (!!))
import Data.Function (const)
import Data.Traversable (Accum, mapAccumL)

import Control.Monad.Aff.Class
import Control.Monad.Reader.Class

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi (MatrixApis, MatrixApi, API,packageByName, singleResult, latestReportByPackageName)
import Lib.Uri
import Lib.Types
import CSS.Display (Display, block, displayNone, display)
import Halogen.HTML.CSS as CSS

type State =
  { logdisplay  :: Display
  , package     :: Package
  , report      :: ShallowReport
  , highlighted :: Boolean
  , logmessage  :: String
  , columnversion :: ColumnVersion
  }

data Query a
  = Initialize a
  | AddingTag TagName a
  | QueueingPackage a
  | FailingPackage a
  | HighlightCell PackageName VersionName VersionName a
  | AddingNewTag a
  | QueueBuild a
  | Finalize a


component :: forall e. H.Component HH.HTML Query Unit Void (MatrixApis e)
component = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

    initialState :: State
    initialState =
      {
        logdisplay: displayNone
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
            , queueing
            , tagging
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
                    [ HP.id_ "package" ] [ (renderTableMatrix state state.package state.report) ]
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
        tagging =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
            [ HH.h4
                [ HP.class_ (H.ClassName "header") ]
                [ HH.text "Tags" ]
            , HH.div
                [ HP.id_ "tagging" ]
                [ HH.ul
                    [ HP.class_ (H.ClassName "tags") ]
                    [] -- TODO : This is where the list of tags generated based on the selected package
                , HH.div
                    [ HP.class_ (H.ClassName "form") ]
                    [ HH.label_
                        [ HH.text "Tag"
                        , HH.input
                            [ HP.type_ HP.InputText
                            , HP.class_ (H.ClassName "tag-name")
                            -- HE.onValueChange ...
                            -- TODO : save the text.
                            ]

                        ]
                    , HH.button
                        [ HP.class_ (H.ClassName "action")
                        , HE.onClick $ HE.input_ AddingNewTag
                        -- TODO : When this button clicked, it will get text then add tag to the <ul class="tags"> above
                        ]
                        [ HH.text "Add Tag" ]
                     ]
                ]
            ]
        queueing =
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
                            [ HP.class_ (H.ClassName "prio") ]
                            [ HH.option
                                [ HP.value "high" ]
                                [ HH.text "High" ]
                            , HH.option
                                [ HP.value "medium"
                                , HP.selected true
                                ]
                                [ HH.text "High" ]
                            , HH.option
                                [ HP.value "low" ]
                                [ HH.text "Low" ]
                            ]
                        ]
                    , HH.button
                        [ HP.class_ (H.ClassName "action")
                        , HE.onClick $ HE.input_ AddingNewTag
                        ]
                        [ HH.text "Queue build for this package" ]
                    ]
                ]
            ]

    eval :: Query ~> H.ComponentDSL State Query Void (MatrixApis e)
    eval (Initialize next) = do
      st <- H.get
      packageByName <- H.lift $ getPackageByName "lens"
      reportPackage <- H.lift $ getLatestReportByPackageName "lens"
      initState <- H.put $ st { package = packageByName, report = reportPackage, highlighted = false}
      pure next
    eval (AddingTag tag next) = do
      pure next
    eval (QueueingPackage next) = do
      pure next
    eval (FailingPackage next) = do
      pure next
    eval (HighlightCell pkgName ghcVer pkgVer next) = do
      singleResult <- H.lift $ getSingleResult pkgName (ghcVer <> "-" <> pkgVer)
      H.modify \st -> st { logmessage = (if ( isContainedLog singleResult.resultA ) then "" else pickLogMessage singleResult )
                         , logdisplay = block
                         , columnversion = { ghcVer, pkgVer }
                         }
      pure next
    eval (AddingNewTag next) = do
      pure next
    eval (QueueBuild next) = do
      pure next
    eval (Finalize next) = do
      pure next

renderMissingPackage :: forall p i. PackageName -> HH.HTML p i
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

renderLogResult :: forall p i. Display -> String -> Package -> ColumnVersion -> HH.HTML p i
renderLogResult logdisplay log { name } { ghcVer, pkgVer } =
  HH.div
    [ HP.id_ "tabs"
    , HP.classes (H.ClassName <$> ["ui-tabs","ui-widget","ui-widget-content","ui-corner-all"])
    , CSS.style $ display logdisplay
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

pickLogMessage :: SingleResult -> String
pickLogMessage { resultA } = logMessage resultA

logMessage :: Maybe VersionResult -> String
logMessage (Just { result }) =
  case result of
    (Fail str) -> str
    _          -> " "
logMessage  _  = " "

isContainedLog :: Maybe VersionResult -> Boolean
isContainedLog versionR = Str.null $ logMessage versionR

renderTableMatrix :: forall p. State -> Package -> ShallowReport -> HH.HTML p (Query Unit)
renderTableMatrix state { name, versions } shallowR@{ packageName, modified, results } =
  HH.table_
    [ HH.thead_
        [ HH.tr_ $
            [ HH.th_
                [ HH.a
                    [ HP.href $ "https://hackage.haskell.org/package/" <> packageName ]
                    [ HH.text packageName ]
                ]
            ] <> ((\x -> HH.th_ $ [HH.text x]) <$> ghcVersions)
        ]
    , HH.tbody_ $ getTheResult accumResult
    ]
  where
    accumResult = mapAccumL (generateTableRow state shallowR) { version: "", revision: 0, preference: Normal } (reverse versions)
    getTheResult { value } = value

generateTableRow :: forall p. State
                 -> ShallowReport
                 -> VersionInfo
                 -> VersionInfo
                 -> Accum VersionInfo (HH.HTML p (Query Unit))
generateTableRow { package, highlighted } { results } prevVer currentVer  =
  { accum: const currentVer prevVer
  , value: HH.tr
             [ HP.classes (H.ClassName <$> (["solver-row"] <> packageVersioning minorCheck majorCheck)) ] $
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
             ] <>  (generateTableColumn highlighted package.name currentVer.version <$> reverse results)
  }
  where
    splitPrevVer = splitVersion prevVer.version
    splitCurrVer = splitVersion currentVer.version
    majorCheck = newMajor splitPrevVer splitCurrVer
    minorCheck = newMinor splitPrevVer splitCurrVer

splitVersion :: VersionName -> Array VersionName
splitVersion v = Str.split (Str.Pattern ".") v

newMajor :: Array VersionName -> Array VersionName -> Boolean
newMajor a b
  | a == [""] =  true
  | a == b    =  false
  | otherwise =  (a !! 0) /= (b !! 0)
              || (fromMaybe "0" (a !! 1) /= fromMaybe "0" (b !! 1))

newMinor :: Array VersionName -> Array VersionName -> Boolean
newMinor a b
  | a == [""] =  true
  | a == b    =  false
  | otherwise =  newMajor a b
              || ((fromMaybe "0" (a !! 2)) /= (fromMaybe "0" (b !! 2)))

packageVersioning :: Boolean -> Boolean  -> Array String
packageVersioning minor major
  | minor && major = ["first-major", "first-minor"]
  | major          = ["first-major"]
  | minor          = ["first-minor"]
  | otherwise      = []

containedRevision :: forall p i. PackageName -> VersionName -> Word -> HH.HTML p i
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

generateTableColumn :: forall p. Boolean
                    -> PackageName
                    -> VersionName
                    -> ShallowGhcResult
                    -> HH.HTML p (Query Unit)
generateTableColumn highlight pkgName verName { ghcVersion, ghcResult } =
    HH.td
      [ HP.classes (H.ClassName <$> (["stcell"] <> (concat $ checkPassOrFail verName <$> ghcResult)))
      , HP.attr (H.AttrName "data-ghc-version") ghcVersion
      , HP.attr (H.AttrName "data-package-version") verName
      , HE.onClick $ HE.input_ (HighlightCell pkgName ghcVersion verName)
      ] $
      [ HH.text (Str.joinWith "" $ checkShallow verName <$> ghcResult) ]
-- TODO: implement highlight similar to the following
-- <> ((\x -> if x then ["highlight"] else [""]) highlight)))

getPackageVersion verName { packageVersion, result }
  | verName == packageVersion && (resultIsFail result) = packageVersion
  | otherwise                                          = ""

resultIsFail :: ShallowResult -> Boolean
resultIsFail sR =
  case sR of
    ShallowFail          -> true
    _                    -> false

checkPassOrFail :: VersionName -> ShallowVersionResult -> Array String
checkPassOrFail verName { packageVersion, result }
  | verName == packageVersion = passOrFail result
  | otherwise                 = []

passOrFail :: ShallowResult -> Array String
passOrFail sR =
  case sR of
    ShallowOk              -> ["pass-build"]
    ShallowNop             -> ["pass-no-op"]
    ShallowNoIp            -> ["pass-no-ip"]
    ShallowNoIpBjLimit _   -> ["fail-bj"]
    ShallowNoIpFail        -> ["fail-no-ip"]
    ShallowFail            -> ["fail-build"]
    ShallowFailDeps _      -> ["fail-dep-build"]
    _                      -> ["fail-unknown"]

checkShallow :: VersionName -> ShallowVersionResult -> String
checkShallow verName { packageVersion, result }
  | verName == packageVersion = checkShallowResult result
  | otherwise                 = ""

checkShallowResult :: ShallowResult -> String
checkShallowResult sR =
  case sR of
    ShallowOk            -> "OK"
    ShallowNop           -> "OK (boot)"
    ShallowNoIp          -> "OK (no-ip)"
    ShallowNoIpBjLimit _ -> "FAIL (BJ)"
    ShallowNoIpFail      -> "FAIL (no-ip)"
    ShallowFail          -> "FAIL (pkg)"
    ShallowFailDeps _    -> "FAIL (deps)"
    _                    -> ""


getLatestReportByPackageName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                             => MonadAff (api :: API | e) m
                             => PackageName
                             -> m (ShallowReport)
getLatestReportByPackageName pkgName = do
  client <- asks _.matrixClient
  liftAff (latestReportByPackageName client pkgName)

getPackageByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                 => MonadAff (api :: API | e) m
                 => PackageName
                 -> m (Package)
getPackageByName pkgName = do
  client <- asks _.matrixClient
  liftAff (packageByName client pkgName)

getSingleResult :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                => MonadAff (api :: API | e) m
                => PackageName
                -> Cell
                -> m (SingleResult)
getSingleResult pkgName cellName = do
  client <- asks _.matrixClient
  liftAff (singleResult client pkgName cellName)

ghcVersions :: Array VersionName
ghcVersions = ["8.2","8.0","7.10","7.8","7.6","7.4"]

hdiffUrl :: PackageName -> VersionInfo -> VersionInfo -> HdiffUrl
hdiffUrl pkgName prevVer currVer
  | Str.null prevVer.version           = "http://hdiff.luite.com/cgit/" <> pkgName <> "/commit?id=" <> currVer.version
  | currVer.version == prevVer.version = "http://hdiff.luite.com/cgit/" <> pkgName <> "/diff?id=" <> currVer.version
  | otherwise                          = "http://hdiff.luite.com/cgit/" <>
                                         pkgName <> "/diff?id=" <>
                                         currVer.version <> "&id2="
                                         <> prevVer.version


hackageUrl :: PackageName -> VersionName -> HackageUrl
hackageUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/" <> pkgName <> ".cabal/edit"

revisionsUrl :: PackageName -> VersionName -> RevisionUrl
revisionsUrl pkgName versionName =
  "https://hackage.haskell.org/package/"  <> pkgName <>  "-" <> versionName <> "/revisions"


cellHash :: VersionName -> PackageName -> VersionName -> Cell
cellHash ghcVer pkgName pkgVer =
  "GHC-" <> ghcVer <> "/" <> pkgName <> "-" <> pkgVer


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
