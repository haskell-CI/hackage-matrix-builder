module Container where

import Prelude
import Data.Char
import Data.Set
import Data.Foldable
import Lib.Uri
import Lib.Undefined
import Lib.MatrixApi
import Lib.MiscFFI
import Control.Monad.Aff.Class
import Control.Monad.Reader.Class
import Control.Monad.Eff.Ref
import Components.PageError as PageError
import Components.PageHome as PageHome
import Components.PageLatest as PageLatest
import Components.PagePackage as PagePackage
import Components.PagePackages as PagePackages
import Components.PageUser as PageUser
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData as RD
import Router as Router
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Debug.Trace (traceAnyA)
import Lib.Types (ApiList, PackageMeta, PageRoute, PageRoute(..), Tag, TagName, PackageName)

type State = {
    route :: PageRoute
  , selectedpackage :: PackageMeta
}

data Query a =
    ReadStates a
  | Initialize a
  | RouteChange PageRoute a

type ChildQuery = Coproduct6 PageError.Query
                             PageHome.Query
                             PageLatest.Query
                             PagePackage.Query
                             PagePackages.Query
                             PageUser.Query

type ChildSlot = Either6 Unit Unit Unit Unit Unit Unit

ui :: forall e. H.Component HH.HTML Query Unit Void (MatrixApis e)
ui =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState =
      { route: HomePage 
      , selectedpackage: { name: ""
                         , report: Nothing
                         , tags: []
                         }
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (MatrixApis e)
    render st =
      HH.div
        [ HP.id_ "container"]
        [ case st.route of
            LatestPage ->
              HH.slot' CP.cp3 unit PageLatest.component unit absurd
            (PackagePage pkgName)->
              HH.slot' CP.cp4 unit PagePackage.component st.selectedpackage absurd
            PackagesPage ->
              HH.slot' CP.cp5 unit PagePackages.component unit absurd
            (UserPage _) ->
              HH.slot' CP.cp6 unit PageUser.component unit absurd
            HomePage ->
              HH.slot' CP.cp2 unit PageHome.component unit absurd
            _ ->
              HH.slot' CP.cp1 unit PageError.component unit absurd
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (MatrixApis e)
    eval (ReadStates next) = do
      pure next
    eval (Initialize next) = do
      pkgList <- H.lift getPackageList
      pkgRef <- asks _.packageList
      liftEff $ writeRef pkgRef (RD.Success pkgList)
      pure next
    eval (RouteChange str next) = do
      packages<- H.query' CP.cp5 unit $ H.request $ PagePackages.CurrentSelected
      case packages of
           (Just pkg) -> H.modify (_ { selectedpackage = pkg })
           Nothing    -> H.modify (_ { selectedpackage = {name: "", report: Nothing, tags: []} })
      H.modify (_ { route = str })
      traceAnyA str
      pure next

getPackageList :: forall e m. MonadReader Environment m
               => MonadAff (api :: API | e) m
               => m (ApiList PackageMeta)
getPackageList = do
  client <- asks _.matrixClient
  liftAff (packageList client { count : (Just 100000), offset : Nothing })

