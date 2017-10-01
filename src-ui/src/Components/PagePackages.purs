module Components.PagePackages where

import Debug.Trace
import Control.Monad.Eff.Ref as Ref
import Data.Array as Arr
import Data.Char as Char
import Data.Foldable as F
import Data.Set as Set
import Data.StrMap as SM
import Data.String as Str
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MatrixApi2 as Api2
import Lib.MiscFFI as Misc
import Lib.Types as T
import Network.RemoteData as RD
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(..), isNothing)
import Prelude (type (~>), Unit, Void, class Ord, append, bind, const, discard, not, otherwise, pure, ($), (<$>), (<<<), (<>), (==), (<=), (&&))
import Control.Monad.Eff.Exception as E


type State =
 {
   packages :: RD.RemoteData E.Error (Array T.PackageName)
 , tags :: RD.RemoteData E.Error (Array T.TagName)
 , tagsMap :: SM.StrMap (Array T.TagName)
 , clicked :: Boolean
 , selectedTag :: Set.Set T.TagName
 , selectedPrefix :: Char
 , latestIdxState :: RD.RemoteData E.Error (SM.StrMap T.PkgIdxTs)
 }

data Query a
  = Initialize a
  | SelectedTag T.TagName a
  | SelectedPrefix Char a
  | HandleCheckBox State Boolean a
  | Finalize a

component :: forall e. H.Component HH.HTML Query Unit Void (Api.Matrix e)
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
     packages: RD.NotAsked
   , tags: RD.NotAsked
   , tagsMap: SM.empty
   , clicked: false
   , selectedTag: Set.empty
   , selectedPrefix: 'A'
   , latestIdxState: RD.NotAsked
   }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.id_ "page-packages"
      , HP.class_ (H.ClassName "page")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "rightcol") ]
          [ HH.div
              [ HP.class_ (H.ClassName "sub") ]
              [ HH.text "Times are shown in your timezone" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "leftcol") ]
          [ HH.h2
              [ HP.class_ (H.ClassName "main-header") ]
              [ HH.text "Packages" ]
          , HH.div
              [ HP.class_ (H.ClassName "main-header-subtext") ]
              []
          , HH.label_
              [ HH.input
                  [ HP.class_ (H.ClassName "packages-only-reports")
                  , HP.type_ HP.InputCheckbox
                  , HE.onChecked $ HE.input (HandleCheckBox state)
                  ]
              , HH.text " Only show packages with reports"
              ]
          , HH.ol
              [ HP.classes (H.ClassName <$> ["tag-filter","clearfix"])
              ] $ ( buildTags' state) <$> (pkgs state.tags)
          , HH.ol
              [ HP.classes (H.ClassName <$> ["headers","clearfix"]) ] $ buildPrefixs <$> prefixs
          , HH.ol
              [ HP.class_ (H.ClassName "packages") ] $ buildPackages state <$> (packages' state)
          ]
      ]
    where
      pkgs (RD.Success a) = a
      pkgs _              = []
      packages' st = ((tagFilter st) <<< (prefixFilter st)) (pkgs state.packages)
      tagFilter {tagsMap, selectedTag} = Arr.filter (tagContained selectedTag tagsMap)
      prefixFilter {selectedPrefix, packages} = Arr.filter (prefixContained selectedPrefix)

  eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
  eval (Initialize next) = do
    st <- H.get
    pkgRef <- asks _.packages
    listPkg <- liftEff $ Ref.readRef pkgRef
    tagList <- H.lift Api2.getTagsWithoutPackage
    tagPkgList <- H.lift Api2.getTagsWithPackages
    traceAnyA "after get tags withPkg api calls"
    lastIdx <- H.lift Api2.getPackagesIdxstate
    traceAnyA "after pkg idx calls"
    let
      pkgTag = case tagPkgList of
        RD.Success a -> a
        _ -> SM.empty
      tagPkgs = pkgTagList pkgTag
    traceAnyA "before get lastpkgIdx"
    traceAnyA "after parse lastpkgidx"
    traceAnyA "after parse pkgArr"
    H.modify  _ { packages = listPkg
                , tags = tagList
                , tagsMap = tagPkgs
                , clicked = false
                , latestIdxState = lastIdx
                }
    traceAnyA "update state latestIdxState"
    traceAnyA "update State"
    -- traceAnyA tagPkgList
    pure next

  eval (SelectedTag tag next) = do
    H.modify \st -> st { selectedTag = if (Set.member tag st.selectedTag)
                                          then Set.delete tag st.selectedTag
                                          else Set.insert tag st.selectedTag }
    pure next

  eval (SelectedPrefix prefix next) = do
    H.modify \st -> st { selectedPrefix = prefix }
    pure next

  eval (HandleCheckBox st isCheck next)
    | isCheck = do
        H.modify _ { packages = RD.NotAsked} -- TODO: get report for Arr.filter indexStateContained
        pure next
    | otherwise = eval (Initialize next)

  eval (Finalize next) = do
    pure next

prefixs :: Array Char
prefixs = Char.fromCharCode <$> ((Arr.(..) 65 90) <> [48])

buildPrefixs :: forall p. Char -> HH.HTML p (Query Unit)
buildPrefixs prefix =
  HH.li_
    [ HH.a
        [ HP.class_ (H.ClassName "header")
        , HP.attr (H.AttrName "data-prefix") (Str.singleton prefix)
        , HE.onClick $ HE.input_ (SelectedPrefix prefix)
        ]
        [ HH.text $ Str.singleton prefix ]
    ]

buildTags :: forall p i. T.TagName -> HH.HTML p i
buildTags tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag
    ]
    [ HH.text $ tag ]

buildTags' :: forall p. State -> T.TagName -> HH.HTML p (Query Unit)
buildTags' st tag =
  HH.a
    [ HP.classes (H.ClassName <$> ["tag-item", clickStatus])
    , HP.attr (H.AttrName "data-tag-name") tag
    , HE.onClick $ HE.input_ (SelectedTag tag)
    ]
    [ HH.text $ tag ]
  where
    clickStatus = if (Set.member tag st.selectedTag)  then "active" else " "

buildPackages :: forall p. State -> T.PackageName -> HH.HTML p (Query Unit)
buildPackages state pkgName =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> pkgName]
        [ HH.text pkgName ]
    ] <> (buildTags <$> (getTheTags state pkgName)) <> (case indexPkg state.latestIdxState of
                                                           Just i -> [ HH.small_ [ HH.text $ " - index-state: " <> i ] ]
                                                           Nothing -> [])
  where
    indexPkg (RD.Success idx) = Misc.toDateTime <$> SM.lookup pkgName idx
    indexPkg _                = Nothing

tagContained :: Set.Set T.TagName -> SM.StrMap (Array T.TagName) -> T.PackageName -> Boolean
tagContained selectedTags tagsMap pkgName
    | Set.isEmpty selectedTags = true
    | otherwise            =
      let
        tags =
          case SM.lookup pkgName tagsMap of
            Just a  -> a
            Nothing -> []
      in not Set.isEmpty (Set.fromFoldable tags `Set.intersection` selectedTags)

prefixContained :: Char -> T.PackageName -> Boolean
prefixContained '0' pkgName = case Str.charAt 0 pkgName of
                                Nothing -> false
                                Just c  -> '0' <= c && c <= '9'
prefixContained selectedPrefix pkgName = case Str.charAt 0 pkgName of
                                           Nothing -> false
                                           Just c  -> selectedPrefix == Char.toUpper c

indexStateContained :: T.PackageMeta -> Boolean
indexStateContained pkgMeta
    | isNothing pkgMeta.report = false
    | otherwise = true

getTheTags :: State -> T.PackageName -> Array T.TagName
getTheTags { tagsMap } pkg =
  case SM.lookup pkg tagsMap of
    Just a -> a
    Nothing -> []

pkgTagList :: SM.StrMap (Array T.PackageName)
           -> SM.StrMap (Array T.TagName)
pkgTagList m =
        SM.fromFoldableWith append $ do
          Tuple.Tuple k vs <- SM.toUnfoldable m
          v <- vs
          pure (Tuple.Tuple v [k])

