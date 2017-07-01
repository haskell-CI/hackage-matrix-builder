module Components.PageError where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lib.MatrixApi (MatrixApis)

type State = {
  

}

data Query a = ReadStates a

component :: forall e. H.Component HH.HTML Query Unit Void (MatrixApis e)
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = {}

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

    eval :: Query ~> H.ComponentDSL State Query Void (MatrixApis e)
    eval (ReadStates next) = do
      pure next
