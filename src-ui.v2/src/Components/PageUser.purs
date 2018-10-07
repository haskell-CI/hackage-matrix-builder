module Components.PageUser where

import Prelude (type (~>), Void, bind, otherwise, pure, ($), (<$>), (<*>), (<>), (==), (>))
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Array as Arr
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Lib.MatrixApi as Api
import Lib.MiscFFI as Misc
import Lib.Types as T
import Data.Tuple as Tuple
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Eff.Ref as Ref
import Network.RemoteData as RD
import Control.Monad.Eff.Exception as E
import Data.StrMap as SM
import Debug.Trace

type State =
 { initUser :: T.Username
 , user :: RD.RemoteData E.Error T.User
 , packages :: Array (Tuple.Tuple T.PackageName T.PkgIdxTs)
 , withReports :: Boolean
 }

data Query a
  = Initialize a
  | Receive T.Username a
  | ToggleWithReports Boolean a
  | Finalize a

component :: forall e. H.Component HH.HTML Query T.Username Void (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: HE.input Receive
  }
  where

  initialState :: T.Username -> State
  initialState usr =
    { initUser: usr
    , user: RD.NotAsked
    , packages: []
    , withReports: false
    }

  render :: State -> H.ComponentHTML Query
  render state = renderBody' state state.user
    where
      renderBody' st (RD.Success user) =
        HH.div
          [ HP.id_ "page-user"
          ,  HP.class_ (H.ClassName "page")
          ]
          [ HH.div
              [ HP.class_ (H.ClassName "leftcol") ] $
              [ HH.h2
                  [ HP.class_ (H.ClassName "main-header") ]
                  [ HH.text $ st.initUser ]
              , HH.div
                  [ HP.class_ (H.ClassName "main-header-subtext") ]
                  [ HH.text "Displaying packages maintained by this user."]
              , HH.div
                  [ HP.class_ (H.ClassName "content") ]
                  [ HH.label_
                      [ HH.input
                          [ HP.class_ (H.ClassName "user-only-reports")
                          , HP.type_ HP.InputCheckbox
                          , HP.checked state.withReports
                          , HE.onChecked $ HE.input ToggleWithReports
                          ]
                      , HH.text "Only show packages with reports"
                      ]
                  , HH.ol
                      [ HP.class_ (H.ClassName "packages") ] $ buildPackages <$> st.packages
                  ]
              ]
          ]

      renderBody' st (RD.NotAsked) =
        HH.div
          [ HP.id_ "page-user"
          ,  HP.class_ (H.ClassName "page")
          ]
          [ HH.div
              [ HP.class_ (H.ClassName "leftcol") ] $
              [ HH.h2
                  [ HP.class_ (H.ClassName "main-header") ]
                  [ HH.text $ st.initUser ]
              , HH.div
                  [ HP.class_ (H.ClassName "main-header-subtext") ]
                  [ HH.text "Displaying packages maintained by this user."]
              , HH.div
                  [ HP.class_ (H.ClassName "content") ]
                  [ HH.label_
                      [ HH.input
                          [ HP.class_ (H.ClassName "user-only-reports")
                          , HP.type_ HP.InputCheckbox
                          , HP.checked state.withReports
                          , HE.onChecked $ HE.input ToggleWithReports
                          ]
                      , HH.text "Only show packages with reports"
                      ]
                  ]
              ]
          ]

      renderBody' st _ =
        HH.div
          [ HP.id_ "page-user"
          ,  HP.class_ (H.ClassName "page")
          ]
          [ HH.div
              [ HP.class_ (H.ClassName "leftcol") ] $
              [ HH.h2
                  [ HP.class_ (H.ClassName "main-header") ]
                  [ HH.text $ state.initUser ]
              , HH.div
                  [ HP.classes (H.ClassName <$> ["main-header-subtext", "error"]) ]
                  [ HH.text "The user could not been found" ]
              ]
          ]


  eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
  eval (Initialize next) = do
    st <- H.get
    lastIdx <-  H.lift Api.getPackagesIdxstate
    selectedUser <- H.lift $ Api.getUser st.initUser

    let idxMap =
          case lastIdx of
            RD.Success a -> a
            _            -> SM.empty
        pkgMeta = userPackageMeta selectedUser (SM.toUnfoldable idxMap)
    initState <- H.put $ st { user = selectedUser
                            , packages = pkgMeta
                            }
    pure next

  eval (ToggleWithReports b next) = do
    _ <- H.modify \st -> st { withReports = b }
    pure next

  eval (Receive userName next) = do
    st <- H.get
    lastIdx <-  H.lift Api.getPackagesIdxstate
    selectedUser <- H.lift $ Api.getUser st.initUser

    let idxMap =
          case lastIdx of
            RD.Success a -> a
            _            -> SM.empty
        pkgMeta = userPackageMeta selectedUser (SM.toUnfoldable idxMap)

    _ <- H.modify _ { initUser = userName
                    , user = selectedUser
                    , packages = pkgMeta
                    }
    pure next

  eval (Finalize next) = do
    pure next

buildPackages :: forall p i. Tuple.Tuple T.PackageName T.PkgIdxTs -> HH.HTML p i
buildPackages (Tuple.Tuple name report) =
  HH.li_ $
    [ HH.a
        [ HP.href $ "/#/package/" <> (name) ]
        [ HH.text (name) ]
    ] <> [ HH.small_ [ HH.text (" - index-state: " <> (Misc.toDateTime report)) ]
         ]

userPackageMeta :: RD.RemoteData E.Error T.User
                -> Array (Tuple.Tuple String T.PkgIdxTs)
                -> Array (Tuple.Tuple String T.PkgIdxTs)
userPackageMeta (RD.Success usr) pkgIdxArr =
  Arr.filter (userPackageIndex usr) pkgIdxArr
userPackageMeta _ _ = []

userPackageIndex :: T.User -> Tuple.Tuple String T.PkgIdxTs -> Boolean
userPackageIndex { packages } (Tuple.Tuple pkgName _) = Arr.elem pkgName packages


