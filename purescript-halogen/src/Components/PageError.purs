module Components.PageError where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Uri

type State = {
  display :: String
  

}

data Query a = ReadStates a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { display: "none" }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.id_ "page-notfound"
	, HP.class_ (H.ClassName "page")
	]
        [ HH.div
            [ HP.class_ (H.ClassName "leftcol") ]
            [ HH.h2
 	        [ HP.class_ (H.ClassName "main-header") ]
	        [ HH.text "404'd!" ]
            , HH.div
	        [ HP.classes (H.ClassName <$> ["main-header-subtext", "error"]) ]
	        [ HH.text "The page could not be found!" ]
            ]
	]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (ReadStates next) = do
      pure next