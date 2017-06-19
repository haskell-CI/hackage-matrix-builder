module Components.PageHome where

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
        [ HP.id_ "page-home"
        , HP.class_ (H.ClassName "page")
        ]
        [ HH.div
            [ HP.class_ (H.ClassName "leftcol") ]
            [ HH.h2
                [ HP.class_ (H.ClassName "main-header") ]
	        [ HH.text "Welcome" ]
            , HH.h3_
	        [ HH.text "Documents" ]
            , HH.ul_
                [ HH.li_
	            [ HH.a
	                [ HP.href "https://github.com/haskell-infra/hackage-trustees/blob/master/policy.md" ]
		        [ HH.text "Hackage trustee policy and procedures" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://wiki.haskell.org/Taking_over_a_package" ]
		        [ HH.text "Wiki: Taking over a package" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://wiki.haskell.org/Hackage_trustees" ]
		        [ HH.text "Wiki: Hackage Trustee" ]
	            ]
	        ]
            , HH.h3_
	        [ HH.text "Trustee Tools" ]
            , HH.ul_
                [ HH.li_
	            [ HH.a
	                [ HP.href "https://github.com/haskell-infra/hackage-trustees/issues" ]
		        [ HH.text "Issue tracker" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://www.github.com/hvr/hackage-matrix-builder" ]
		        [ HH.text "hackage-matrix-builder source" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://www.github.com/hvr/hackage-cli" ]
		        [ HH.text "hackage-cli" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://github.com/hackage-trustees" ]
		        [ HH.text "Github organization" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "http://hackage.haskell.org/packages/recent/revisions.html" ]
		        [ HH.text "Recent Revisions" ]
	            ] 
	        ]
            , HH.h3_
	        [ HH.text "References" ]
            , HH.ul_
                [ HH.li_
	            [ HH.a
	                [ HP.href "https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory" ]
		        [ HH.text "GHC Boot Library Version History" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://ghc.haskell.org/trac/ghc/wiki/LanguagePragmaHistory" ]
		        [ HH.text "Language Pragma History" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://github.com/haskell/cabal/blob/641e854ae663e2b34f34ecc11ba663ac3a9bdc19/Cabal/Distribution/PackageDescription/Check.hs#L911-L1091" ]
		        [ HH.text "Required cabal-version" ]
	            ]
	        , HH.li_
	            [ HH.a
	                [ HP.href "https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md" ]
		        [ HH.text "Cookbook for common build failures" ]
	            ]
	        ]
            ]   
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (ReadStates next) = do
      pure next