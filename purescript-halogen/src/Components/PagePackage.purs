module Components.PagePackage where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.Uri

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
        [ HP.id_ "page-package"
	, HP.class_ (H.ClassName "page")
	]
        [ HH.div 
            [ HP.class_ (H.ClassName "rightcol") ]
            [ legend
            , queueing 
            , tagging
            ]
        , HH.div
            [ HP.class_ (H.ClassName "leftcol") ]
            [ HH.h2
	        [ HP.class_ (H.ClassName "main-header") ]
		[]
            -- TODO :  <div class="main-header-subtext error" id="package-not-built">This package doesn't have a build report yet. 
            --         You can request a report by <a href="https://github.com/hvr/hackage-matrix-builder/issues/32">
            --         leaving a comment here</a>.</div>
            -- 
            , HH.div
                [ HP.classes (H.ClassName <$> ["main-header-subtext","last-build"]) ]
		[]
            , HH.div
                [ HP.id_ "package-buildreport" ]
                [ HH.h3
                    [ HP.class_ (H.ClassName "package-header") ]
                    [ HH.text "Solver Matrix (constrained by single version) " ]
                , HH.div
                    [ HP.id_ "package" ]
                    []
                    -- TODO : All the package matrix table goes here.
                , HH.h3
                    [ HP.class_ (H.ClassName "logs-header") ]
                    [ HH.text "Logs" ]
                , HH.div
                    [ HP.id_ "tabs-container" ]
                    []
                    -- TODO : All the logs report from the build table goes here (when the column clicked). 
                    -- From : https://matrix.hackage.haskell.org/package/AhoCorasick
                    -- To :  https://matrix.hackage.haskell.org/package/AhoCorasick#GHC-7.10/AhoCorasick-0.0.3
                ]
            ]
	]
      where
        tagging =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
	    [ HH.h4
	        [ HP.class_ (H.ClassName "header") ]
	        [ HH.text "Tags" ]
	    , HH.div
	        [ HP.id_ "tagging" ]
	        [ HH.ul
	            [ HP.class_ (H.ClassName "tags") ]
		    [] -- TODO : This is where the list of tags generated based on the selected package
	        , HH.div
	            [ HP.class_ (H.ClassName "form") ]
		    [ HH.label_
		        [ HH.text "Tag"
		        , HH.input
		            [ HP.type_ HP.InputText
			    , HP.class_ (H.ClassName "tag-name")
			    -- HE.onValueChange ...
			    -- TODO : save the text.
			    ]
			    
		        ]
		    , HH.button
		        [ HP.class_ (H.ClassName "action")
                        -- HE.onClick (HE.input ...)
		        -- TODO : When this button clicked, it will get text then add tag to the <ul class="tags"> above
                        ]
		        [ HH.text "Add Tag" ]
		     ]
	        ]
	    ] 
        queueing =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
	    [ HH.h4
	        [ HP.class_ (H.ClassName "header") ]
	        [ HH.text "Queueing" ]
	    , HH.div
	        [ HP.id_ "queueing" ]
	        [ HH.div
	            [ HP.class_ (H.ClassName "form") ]
		    [ HH.label_
		        [ HH.text "Priority"
		        , HH.select
		            [ HP.class_ (H.ClassName "prio") ]
			    [ HH.option
			        [ HP.value "high" ]
			        [ HH.text "High" ]
			    , HH.option
			        [ HP.value "medium"
			        , HP.selected true
			        ]
			        [ HH.text "High" ]
			    , HH.option
			        [ HP.value "low" ]
			        [ HH.text "Low" ]
			    ]
		        ]
		    , HH.button
		        [ HP.class_ (H.ClassName "action") ]
		        [ HH.text "Queue build for this package" ]
		    ]
	        ]
	    ]  
        legend =
          HH.div
            [ HP.class_ (H.ClassName "sub") ]
	    [ HH.h4
	        [ HP.id_ "legend"
	        , HP.class_ (H.ClassName "header") ]
	        [ HH.text "Legend" ]
	    , HH.table
	        [ HP.id_ "legend-table" ]
	        [ HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["pass-build", "stcell"]) ]
		        [ HH.text "OK" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "package build succesful" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["pass-no-op", "stcell"]) ]
		        [ HH.text "OK (boot)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "pre-installed version" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["pass-no-ip", "stcell"]) ]
		        [ HH.text "OK (no-ip)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "no install-plan found" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["fail-bj", "stcell"]) ]
		        [ HH.text "FAIL (BJ)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "backjump limit reached" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["fail-build", "stcell"]) ]
		        [ HH.text "FAIL (pkg)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "package failed to build" ]
		    ]
                , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["fail-dep-build", "stcell"]) ]
		        [ HH.text "FAIL (deps)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "package dependencies failed to build" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["fail-no-ip", "stcell"]) ]
		        [ HH.text "FAIL (no-ip)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "something went horribly wrong" ]
		    ]
	        , HH.tr_
	            [ HH.td
		        [ HP.classes (H.ClassName <$> ["fail-unknown", "stcell"]) ]
		        [ HH.text "FAIL (unknown)" ]
                    , HH.td
		        [ HP.class_ (H.ClassName "text") ]
		        [ HH.text "test-result missing" ]
		    ] 
	        ]
	    ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (ReadStates next) = do
      pure next