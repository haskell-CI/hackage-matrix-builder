module Components.PageUser where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup ((<>))
import Data.Traversable (Accum, mapAccumL)
import Data.Show (show)
import Data.Array

import Control.Monad.Aff.Class
import Control.Monad.Reader.Class
import Control.Monad.Eff.Ref

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi
import Lib.Uri
import Lib.Types (PackageName, PackageMeta, User, Username, ApiList)
import Lib.MiscFFI
import CSS.Display (Display, display, block, displayNone)
import Halogen.HTML.CSS as CSS

type State =
 {
   display :: Display
 , user :: Username
 , packages :: Array PackageMeta
 }

data Query a
  = Initialize a
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
    { display: displayNone
    , user: ""
    , packages: []
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.id_ "page-user"
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
              [ HH.text $ state.user ]
          , HH.div
              [ HP.class_ (H.ClassName "main-header-subtext") ]
              [ HH.text "Displaying packages maintained by this user."]
          , HH.div
              [ HP.class_ (H.ClassName "content") ]
              [ HH.label_
                  [ HH.input
                      [ HP.class_ (H.ClassName "user-only-reports")
                      , HP.type_ HP.InputCheckbox
                      ]
                  , HH.text "Only show packages with reports"
                  ]
              , HH.ol
                  [ HP.class_ (H.ClassName "packages") ] $ buildPackages <$> state.packages

              ]
          ]
      ]


  eval :: Query ~> H.ComponentDSL State Query Void (MatrixApis e)
  eval (Initialize next) = do
    st <- H.get
    pkglist <- H.lift getPackageList
    usr <- H.lift $ getUserByName "BenGamari"
    initState <- H.put $ st { display = block, user = usr.name, packages = userPackageMeta usr pkglist }
    pure next
   where
    userPackageMeta usr pkglist = concat (filterUserPackage <$> usr.packages <*> pkglist.items)

  eval (SelectedPackage pkgName next) = do
    pure next
  eval (Finalize next) = do
    pure next

getPackageList :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
               => MonadAff (api :: API, ref :: REF | e) m
               => m (ApiList PackageMeta)
getPackageList = do
  client <- asks _.matrixClient
  liftAff (packageList client { count : (Just 100000), offset : Nothing })

getUserByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
              => MonadAff (api :: API | e) m
              => Username
              -> m (User)
getUserByName user = do
  client <- asks _.matrixClient
  liftAff (userByName client user )

buildPackages :: forall p i. PackageMeta -> HH.HTML p i
buildPackages pkgMeta =
  HH.li_ $
    [ HH.a
        [ -- HP.href $ "/package/" <> (pkgMeta.name) -- all of the package's name will goes here
        -- TODO: The action onClick will be added here to direct user to package's page
        ]
        [ HH.text (pkgMeta.name) ]
    ] <> [ HH.small_ [ HH.text $ if reportExist then "" else " - index-state: " <> (formatDate pkgMeta.report) ] ]
  where
    reportExist = pkgMeta.report == Nothing


filterUserPackage :: PackageName -> PackageMeta -> Array PackageMeta
filterUserPackage pkgName pkgMeta
  | pkgName == pkgMeta.name = [ pkgMeta ]
  | otherwise               = []

isEmptyMeta :: Maybe PackageMeta -> Boolean
isEmptyMeta pkgMeta =
  case pkgMeta of
    Just pkgMeta -> true
    Nothing      -> false
