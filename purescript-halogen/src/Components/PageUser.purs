module Components.PageUser where

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
          [ HP.class_ (H.ClassName "leftcol") ]
          [ HH.h2
              [ HP.class_ (H.ClassName "main-header") ]
	      []
          , HH.div
	      [ HP.class_ (H.ClassName "main-header-subtext") ]
	      []
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
	          [ HP.class_ (H.ClassName "packages") ]
	          []
	      ]	      
          ]
      ]
    

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (ReadStates next) = do
    pure next