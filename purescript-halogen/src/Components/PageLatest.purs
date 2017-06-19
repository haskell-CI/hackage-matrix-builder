module Components.PageLatest where

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
        [ HP.id_ "page-latest"
	, HP.class_ (H.ClassName "page")
	]
        [ HH.div
            [ HP.class_ (H.ClassName "rightcol") ]
            [ HH.div
                [ HP.class_ (H.ClassName "sub") ]
	        [ HH.text "Times are shown in your timezone" ]
            , HH.div
                [ HP.class_ (H.ClassName "sub") ]
	        [ HH.button
	            [ HP.class_ (H.ClassName "refresh")
	            , HP.title "Refresh listings"
	            ]
	            [ HH.text "Hello World" ]
	        ]   
            ]
        , HH.div 
            [ HP.classes (H.ClassName <$> ["leftcol", "col-2"])]
            [ HH.div 
                [ HP.class_ (H.ClassName "leftcol-2-left")]
	        [ HH.h2
	            [ HP.class_ (H.ClassName "main-header") ]
	            [ HH.text "Latest Builds" ]
	        , HH.ul
	            [ HP.id_ "build-list" ]
		    []
                ]
            , HH.div
	        [ HP.class_ (H.ClassName "leftcol-2-right") ]
	        [ HH.h2
	            [ HP.class_ (H.ClassName "main-header") ]
	            [ HH.text "Build Queue" ]
	        , HH.table
	            [ HP.id_ "queue-list" ]
		    []
                ]
            ]
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (ReadStates next) = do
      pure next