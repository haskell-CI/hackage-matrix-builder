module Container where

import Prelude

import CSS.Display (Display, displayNone)
import Components.PageError as PageError
import Components.PageHome as PageHome
import Components.PageLatest as PageLatest
import Components.PagePackage as PagePackage
import Components.PagePackages as PagePackages
import Components.PageUser as PageUser
import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceAnyA)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lib.MatrixApi (MatrixApis)

type State = {
  display :: Display
}

data Query a =
    ReadStates a
  | RouteChange String a

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
    , initializer: Nothing -- Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState = {
      display : displayNone
    }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (MatrixApis e)
    render st =
      HH.div
        [ HP.id_ "container"]
	[ HH.slot' CP.cp1 unit PageError.component unit absurd
	, HH.slot' CP.cp2 unit PageHome.component unit absurd
	, HH.slot' CP.cp3 unit PageLatest.component unit absurd
	, HH.slot' CP.cp4 unit PagePackage.component unit absurd
	, HH.slot' CP.cp5 unit PagePackages.component unit absurd
	, HH.slot' CP.cp6 unit PageUser.component unit absurd
	]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (MatrixApis e)
    eval (ReadStates next) = do
      pure next

    eval (RouteChange str next) = do
      traceAnyA str
      pure next
