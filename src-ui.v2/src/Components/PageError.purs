module Components.PageError where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api

type State = {


}

data Query a = ReadStates a

component :: forall e. H.Component HH.HTML Query Unit Void (Api.Matrix e)
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
        [ HP.id_ "page-home"
        , HP.class_ (H.ClassName "page")
        ]
        []

    eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
    eval (ReadStates next) = do
      pure next
