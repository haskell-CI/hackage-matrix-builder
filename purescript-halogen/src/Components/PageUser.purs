module Components.PageUser where

import Prelude (type (~>), Unit, Void, bind, const, otherwise, pure, ($), (<$>), (<*>), (<>), (==), (&&))
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Lib.MatrixApi as Api
import Lib.MiscFFI as M
import Lib.Types as T
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Eff.Ref as Ref
import Network.RemoteData as RD

type State =
 { initUser :: T.Username
 , user :: T.Username
 , packages :: Array T.PackageMeta
 }

data Query a
  = Initialize a
  | HandleCheckBox State Boolean a
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
    pkgRef <- asks _.packageList
    packageList <- liftEff (Ref.readRef pkgRef)
    let pkglist = RD.withDefault {offset: 0, count: 0, items: []} packageList
    selectedUser <- H.lift $ Api.getUserByName st.initUser
    initState <- H.put $ st { user = selectedUser.name, packages = userPackageMeta selectedUser pkglist }
    pure next
   where
    userPackageMeta usr pkglist = Arr.concat (filterUserPackage <$> usr.packages <*> pkglist.items)

  eval (HandleCheckBox st isCheck next)
      | isCheck = do
          _ <- H.modify _ { packages = Arr.filter indexStateContained st.packages}
          pure next
      | otherwise = eval (Initialize next)

  eval (Finalize next) = do
    pure next

renderUserPackages :: forall p. State -> Array (HH.HTML p (Query Unit))
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
                            , HE.onChecked $ HE.input (HandleCheckBox st)
                            ]
                         , HH.text "Only show packages with reports"
                        ]
                    , HH.ol
                        [ HP.class_ (H.ClassName "packages") ] $ buildPackages <$> st.packages
                    ]
                ]

buildPackages :: forall p i. T.PackageMeta -> HH.HTML p i
buildPackages pkgMeta =
  HH.li_ $
    [ HH.a
        [ HP.href $ "/#/package/" <> (pkgMeta.name) ]
        [ HH.text (pkgMeta.name) ]
    ] <> [ HH.small_ [ HH.text $ if reportExist
                                 then " - index-state: "
                                 else " - index-state: " <> (M.formatDate pkgMeta.report)
                     ]
         ]
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

indexStateContained :: T.PackageMeta -> Boolean
indexStateContained pkgMeta
    | isNothing pkgMeta.report = false
    | otherwise = true
