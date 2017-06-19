module Container where

import Prelude

import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.Uri


import Components.PageError    as PageError
import Components.PageHome     as PageHome
import Components.PageLatest   as PageLatest
import Components.PagePackage  as PagePackage
import Components.PagePackages as PagePackages
import Components.PageUser     as PageUser

type State = {
  display :: String
  

}

data Query a = ReadStates a

type ChildQuery = Coproduct6 PageError.Query
                             PageHome.Query
			     PageLatest.Query
			     PagePackage.Query
			     PagePackages.Query
			     PageUser.Query

type ChildSlot = Either6 Unit Unit Unit Unit Unit Unit

ui :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
ui =
  H.lifecycleParentComponent
    {
      initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing -- Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState = {
      display : "none"
    }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
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

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (ReadStates next) = do
      pure next

