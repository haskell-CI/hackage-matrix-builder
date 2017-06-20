module Router where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = { history :: Array String }

data Query a = ChangeRoute String a

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { history: [] }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.nav
        [ HP.id_ "menu"
	, HP.class_ (H.ClassName "clearfix")
	]
	[ HH.div
	    [ HP.classes (H.ClassName <$> ["item","left","logo-container","clearfix"]) ]
	    [ HH.img
	        [ HP.class_ (H.ClassName "logo")
		, HP.src "//www.haskell.org/static/img/logo.png"
		, HP.alt "Haskell Logo"
                ]
            , HH.h1
                [ HP.class_ (H.ClassName "logo-text") ]
                [ HH.text "Hackage Matrix Builder"
                , HH.i_ [ HH.text "3rd"]
                ]
	    ]
        , HH.div
	    [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
	    [ HH.a [ HP.href "#" ] [ HH.text "Home" ] ]
        , HH.div
	    [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
	    [ HH.a [ HP.href "#latest" ] [ HH.text "Latest builds" ] ]
        , HH.div
	    [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
	    [ HH.a [ HP.href "#packages" ] [ HH.text "Packages" ] ]
        , HH.div
	    [ HP.classes (H.ClassName <$> ["item","search","right","clearfix"]) ]
	    [ HH.div
	        [ HP.class_ (H.ClassName "text") ]
		[ HH.text "Package Search" ]
	    , HH.input
		[ HP.type_ HP.InputText
	        , HP.class_ (H.ClassName "input")
		, HP.id_ "search"
		]
            ]
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      ChangeRoute msg next -> do
        -- H.modify \st -> { history: st.history `A.snoc` msg }
        pure next