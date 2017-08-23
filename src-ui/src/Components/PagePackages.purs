module Components.PagePackages where

import Control.Monad.Eff.Ref as Ref
import Data.Array as Arr
import Data.Char as Char
import Data.Set as Set
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MiscFFI as MiscFFI
import Lib.Types as T
import Network.RemoteData as RD
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(..), isNothing)
import Prelude ( type (~>), Unit, Void, bind, const, discard, not, otherwise, pure, ($), (<$>), (<<<), (<>))

type State =
 {
   packages :: Array T.PackageMeta
 , tags :: Array T.Tag
 , clicked :: Boolean
 , selectedTag :: Set.Set T.TagName
 , selectedPrefix :: Set.Set T.Prefixs
 }

data Query a
  = Initialize a
  | SelectedTag T.TagName a
  | SelectedPrefix T.Prefixs a
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
     packages: []
   , tags: []
   , clicked: false
   , selectedTag: Set.empty
   , selectedPrefix: Set.empty
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
              ] $ ( buildTags' state) <$> state.tags
          , HH.ol
              [ HP.classes (H.ClassName <$> ["headers","clearfix"]) ] $ buildPrefixs <$> prefixs
          , HH.ol
              [ HP.class_ (H.ClassName "packages") ] $
                Arr.take 650 $ buildPackages <$> packages'
          ]
      ]
    where
      packages' = (tagFilter <<< prefixFilter) state.packages
      tagFilter = Arr.filter (tagContained state.selectedTag)
      prefixFilter = Arr.filter (prefixContained state.selectedPrefix)

  eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
  eval (Initialize next) = do
    st <- H.get
    tagItem <- H.lift Api.getTagList
    pkgRef <- asks _.packageList
    packageList <- liftEff (Ref.readRef pkgRef)
    let packages = RD.withDefault {offset: 0, count: 0, items: []} packageList
    initState <- H.put $ st { packages = packages.items, tags = tagItem.items, clicked = false}
    pure next

  eval (SelectedTag tag next) = do
    H.modify \st -> st { selectedTag = if (Set.member tag st.selectedTag)
                                          then Set.delete tag st.selectedTag
                                          else Set.insert tag st.selectedTag }
    pure next

  eval (SelectedPrefix prefix next) = do
    H.modify \st -> st { selectedPrefix = Set.singleton prefix }
    pure next

  eval (HandleCheckBox st isCheck next)
    | isCheck = do
        H.modify _ { packages = Arr.filter indexStateContained st.packages}
        pure next
    | otherwise = eval (Initialize next)

  eval (Finalize next) = do
    pure next

prefixs :: Array T.Prefixs
prefixs = Str.singleton <$> Char.fromCharCode <$> (Arr.(..) 65 90)

buildPrefixs :: forall p. String -> HH.HTML p (Query Unit)
buildPrefixs prefix =
  HH.li_
    [ HH.a
        [ HP.class_ (H.ClassName "header")
        , HP.attr (H.AttrName "data-prefix") prefix
        , HE.onClick $ HE.input_ (SelectedPrefix prefix)
        ]
        [ HH.text $ prefix ]
    ]

buildTags :: forall p i. T.TagName -> HH.HTML p i
buildTags tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag
    ]
    [ HH.text $ tag ]

buildTags' :: forall p. State -> T.Tag -> HH.HTML p (Query Unit)
buildTags' st tag =
  HH.a
    [ HP.classes (H.ClassName <$> ["tag-item", clickStatus])
    , HP.attr (H.AttrName "data-tag-name") tag.name
    , HE.onClick $ HE.input_ (SelectedTag tag.name)
    ]
    [ HH.text $ tag.name ]
  where
    clickStatus = if (Set.member tag.name st.selectedTag)  then "active" else " "

buildPackages :: forall p. T.PackageMeta -> HH.HTML p (Query Unit)
buildPackages packageMeta =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> packageMeta.name
        ]
        [ HH.text packageMeta.name ]
    ] <> (buildTags <$> packageMeta.tags) <> [ HH.small_ [ HH.text $ " - index-state: " <> (MiscFFI.formatDate packageMeta.report) ] ]

tagContained :: Set.Set T.TagName -> T.PackageMeta -> Boolean
tagContained selectedTags { tags }
    | Set.isEmpty selectedTags = true
    | otherwise            = not Set.isEmpty (Set.fromFoldable tags `Set.intersection` selectedTags)

prefixContained :: Set.Set T.Prefixs -> T.PackageMeta -> Boolean
prefixContained selectedPrefix { name }
    | Set.isEmpty selectedPrefix = true
    | otherwise              = Set.member (Str.toUpper $ Str.take 1 name) selectedPrefix

indexStateContained :: T.PackageMeta -> Boolean
indexStateContained pkgMeta
    | isNothing pkgMeta.report = false
    | otherwise = true
