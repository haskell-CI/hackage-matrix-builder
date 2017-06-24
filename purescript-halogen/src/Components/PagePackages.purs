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
import Data.Array ((..))
import Data.String as Str
import Data.Char
import Lib.Uri
import Lib.Types (PackageName, PackageMeta, Tag, TagName, ApiList)
import Lib.Undefined
import Lib.MatrixApi
import CSS.Display (Display, block, displayNone, display)
import Halogen.HTML.CSS as CSS

import Network.HTTP.Affjax (AJAX)

type State =
 {
   display :: Display
 , packages :: Array PackageMeta
 , tags :: Array Tag
 }

data Query a
  = Initialize a
  | SelectedTag a
  | SelectedPrefix a
  | PackageList a
  | Finalize a
  | ReadStates a
  
component :: forall e. H.Component HH.HTML Query Unit Void (MyMatrixApi e)
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
     display: block
   , packages: []
   , tags: []
   }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.id_ "page-packages"
      , HP.class_ (H.ClassName "page")
      , CSS.style $ display state.display
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
              [ HP.classes (H.ClassName <$> ["tag-filter","clearfix"]) ] $ do
	          tags <- getTagList
		  buildTags' <$> state.tags
	      -- TODO: This will generate list of tags avaliable using tagList
          , HH.ol
	      [ HP.classes (H.ClassName <$> ["headers","clearfix"]) ] $ buildPrefixs <$> prefixs
	      -- TODO: This will generate Character based sorting
          , HH.ol
	      [ HP.class_ (H.ClassName "packages") ]
	      []
	      -- TODO: This will generates all the packages based on tag-filter or alphabetically order using packageList
          ]
      ]

  eval :: forall e . Query ~> H.ComponentDSL State Query Void (MyMatrixApi e)
  eval (ReadStates next) = do
    pure next
  eval (Initialize next) = do
    st <- H.get
    tagItem <- getTagList
    pkg <- getPackageList
    H.put $ st { display = block, packages = pkg.items, tags = tagItem.items}
    pure next
    
  eval (SelectedTag next) = do
    pure next
  eval (SelectedPrefix next) = do
    pure next
  eval (PackageList next) = do
    pure next
  eval (Finalize next) = do
    pure next

prefixs :: Array String
prefixs = Str.singleton <$> fromCharCode <$> (65 .. 90)

buildPrefixs :: forall p i. String -> HH.HTML p i
buildPrefixs prefix =
  HH.li_
    [ HH.a
        [ HP.attr (H.AttrName "data-prefix") prefix
	, HP.class_ (H.ClassName "header") 
        , HP.href ""
	-- TODO: The action onClick will be added here
        ]
        [ HH.text $ prefix ]
    ]

buildTags :: forall p i. TagName -> HH.HTML p i
buildTags tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag
    , HP.href ""
    ]
    [ HH.text $ tag ]

buildTags' :: forall p i. Tag -> HH.HTML p i
buildTags' tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag.name
    , HP.href ""
    ]
    [ HH.text $ tag.name ]

buildPackages :: forall p i. PackageMeta -> HH.HTML p i
buildPackages packageMeta =
  HH.li_
    [ HH.a
        [ HP.href $ "/package/" <> packageMeta.name -- all of the package's name will goes here
	-- TODO: The action onClick will be added here to direct user to package's page
        ] <> (buildTags <$> packageMeta.tags)
        [ HH.text $ packageMeta.name ]
    ]

getTagList :: forall e m. MonadReader { matrixClient :: MatrixApi } m => MonadAff (api :: API | e) m => m (ApiList Tag)
getTagList = do
  client <- asks _.matrixClient
  liftAff (tagList client)

getPackageList :: forall e m. MonadReader { matrixClient :: MatrixApi } m => MonadAff (api :: API | e) m => m (ApiList PackageMeta)
getPackageList = do
  client <- asks _.matrixClient
  liftAff (packageList client { count : (Just 100000), offset : Nothing })