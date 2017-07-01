module Components.PagePackages where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))

import Control.Monad.Aff.Class
import Control.Monad.Reader.Class

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as Arr
import Data.String as Str
import Data.Char
import Data.Set
import Data.Foldable
import Data.Semigroup ((<>))
import Lib.Uri
import Lib.Types (PackageName, PackageMeta, Tag, TagName, ApiList)
import Lib.Undefined
import Lib.MatrixApi
import Lib.MiscFFI
import CSS.Display (Display, block, displayNone, display)
import Halogen.HTML.CSS as CSS

type State =
 {
   packages :: Array PackageMeta
 , tags :: Array Tag
 , clicked :: Boolean
 , selectedTag :: Set TagName
 , selectedPrefix :: Set Prefixs
 }

type Prefixs = String

data Query a
  = Initialize a
  | SelectedTag TagName a
  | SelectedPrefix Prefixs a
  | SelectedPackage PackageName a
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
     packages: []
   , tags: []
   , clicked: false
   , selectedTag: empty
   , selectedPrefix: empty
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
                  ]
              , HH.text " Only show packages with reports"
              ]
          , HH.ol
              [ HP.classes (H.ClassName <$> ["tag-filter","clearfix"])
              ] $ ( buildTags' state) <$> state.tags
              -- TODO: This will generate list of tags avaliable using tagList
          , HH.ol
              [ HP.classes (H.ClassName <$> ["headers","clearfix"]) ] $ buildPrefixs <$> prefixs
              -- TODO: This will generate Character based sorting
          , HH.ol
              [ HP.class_ (H.ClassName "packages") ] $
                Arr.take 650 $ buildPackages <$> packages'
          ]
      ]
    where
      packages' = (tagFilter <<< prefixFilter) state.packages
      tagFilter = Arr.filter (tagContained state.selectedTag)
      prefixFilter = Arr.filter (prefixContained state.selectedPrefix)



  eval :: Query ~> H.ComponentDSL State Query Void (MatrixApis e)
  eval (Initialize next) = do
    st <- H.get
    tagItem <- H.lift getTagList
    pkg <- H.lift getPackageList
    initState <- H.put $ st { packages = pkg.items, tags = tagItem.items, clicked = false}
    pure next

  eval (SelectedTag tag next) = do
    H.modify \st -> st { selectedTag = if (member tag st.selectedTag)
                                          then delete tag st.selectedTag
                                          else insert tag st.selectedTag }
    pure next

  eval (SelectedPrefix prefix next) = do
    H.modify \st -> st { selectedPrefix = singleton prefix }
    pure next
  eval (SelectedPackage pkgName next) = do
    pure next
  eval (Finalize next) = do
    pure next

prefixs :: Array String
prefixs = Str.singleton <$> fromCharCode <$> (Arr.(..) 65 90)

buildPrefixs :: forall p. String -> HH.HTML p (Query Unit)
buildPrefixs prefix =
  HH.li_
    [ HH.a
        [ HP.class_ (H.ClassName "header")
        , HP.attr (H.AttrName "data-prefix") prefix
        , HE.onClick $ HE.input_ (SelectedPrefix prefix)
        -- TODO: The action onClick will be added here
        ]
        [ HH.text $ prefix ]
    ]

buildTags :: forall p i. TagName -> HH.HTML p i
buildTags tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag
    ]
    [ HH.text $ tag ]

buildTags' :: forall p. State -> Tag -> HH.HTML p (Query Unit)
buildTags' st tag =
  HH.a
    [ HP.classes (H.ClassName <$> ["tag-item", clickStatus])
    , HP.attr (H.AttrName "data-tag-name") tag.name
    , HE.onClick $ HE.input_ (SelectedTag tag.name)
    ]
    [ HH.text $ tag.name ]
  where
    clickStatus = if (member tag.name st.selectedTag)  then "active" else " "

buildPackages :: forall p i. PackageMeta -> HH.HTML p i
buildPackages packageMeta =
  HH.li_ $
    [ HH.a
        [ -- HP.href $ "/package/" <> packageMeta.name -- all of the package's name will goes here
        -- TODO: The action onClick will be added here to direct user to package's page
        ]
        [ HH.text packageMeta.name ]
    ] <> (buildTags <$> packageMeta.tags) <> [ HH.small_ [ HH.text $ " - index-state: " <> (formatDate packageMeta.report) ] ]

getTagList :: forall a e m. MonadReader { matrixClient :: MatrixApi | a} m
           => MonadAff (api :: API | e) m
           => m (ApiList Tag)
getTagList = do
  client <- asks _.matrixClient
  liftAff (tagList client)

getPackageList :: forall a e m. MonadReader { matrixClient :: MatrixApi | a} m
               => MonadAff (api :: API | e) m
               => m (ApiList PackageMeta)
getPackageList = do
  client <- asks _.matrixClient
  liftAff (packageList client { count : (Just 100000), offset : Nothing })

tagContained :: Set TagName -> PackageMeta -> Boolean
tagContained selectedTags { tags }
    | isEmpty selectedTags = true
    | otherwise            = not isEmpty (fromFoldable tags `intersection` selectedTags)

prefixContained :: Set Prefixs -> PackageMeta -> Boolean
prefixContained selectedPrefix { name }
    | isEmpty selectedPrefix = true
    | otherwise              = member (Str.toUpper $ Str.take 1 name) selectedPrefix
