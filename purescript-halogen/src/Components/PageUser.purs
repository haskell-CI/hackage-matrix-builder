module Components.PageUser where

import Prelude (type (~>), Void, bind, const, otherwise, pure, ($), (<$>), (<*>), (<>), (==), (&&))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MiscFFI as M
import Lib.Types as T

type State =
 { initUser :: T.Username
 , user :: T.Username
 , packages :: Array T.PackageMeta
 }

data Query a
  = Initialize a
  | SelectedPackage T.PackageName a
  | Finalize a

component :: forall e. H.Component HH.HTML Query T.Username Void (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  initialState :: T.Username -> State
  initialState usr =
    { initUser: usr
    , user: ""
    , packages: []
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.id_ "page-user"
      , HP.class_ (H.ClassName "page")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "rightcol") ]
          [ HH.div
              [ HP.class_ (H.ClassName "sub") ]
              [ HH.text "Times are shown in your timezone" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "leftcol") ] $
          [ HH.h2
              [ HP.class_ (H.ClassName "main-header") ]
              [ HH.text $ state.initUser ]
          ] <> renderUserPackages state
      ]


  eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
  eval (Initialize next) = do
    st <- H.get
    pkglist <- H.lift Api.getPackageList
    selectedUser <- H.lift $ Api.getUserByName st.initUser
    initState <- H.put $ st { user = selectedUser.name, packages = userPackageMeta selectedUser pkglist }
    pure next
   where
    userPackageMeta usr pkglist = Arr.concat (filterUserPackage <$> usr.packages <*> pkglist.items)

  eval (SelectedPackage pkgName next) = do
    pure next
  eval (Finalize next) = do
    pure next

renderUserPackages :: forall p i. State -> Array (HH.HTML p i)
renderUserPackages st
  | Arr.null st.packages && Str.null st.user = [HH.div
                                                 [ HP.classes (H.ClassName <$> ["main-header-subtext", "error"]) ]
                                                 [ HH.text "The user could not been found" ]]
  | otherwise = [HH.div
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
                        [ HP.class_ (H.ClassName "packages") ] $ buildPackages <$> st.packages ]]

buildPackages :: forall p i. T.PackageMeta -> HH.HTML p i
buildPackages pkgMeta =
  HH.li_ $
    [ HH.a
        [ HP.href $ "/#/package/" <> (pkgMeta.name) ]
        [ HH.text (pkgMeta.name) ]
    ] <> [ HH.small_ [ HH.text $ if reportExist then "" else " - index-state: " <> (M.formatDate pkgMeta.report) ] ]
  where
    reportExist = pkgMeta.report == Nothing


filterUserPackage :: T.PackageName -> T.PackageMeta -> Array T.PackageMeta
filterUserPackage pkgName pkgMeta
  | pkgName == pkgMeta.name = [ pkgMeta ]
  | otherwise               = []

isEmptyMeta :: Maybe T.PackageMeta -> Boolean
isEmptyMeta pkgMeta =
  case pkgMeta of
    Just a -> true
    Nothing      -> false
