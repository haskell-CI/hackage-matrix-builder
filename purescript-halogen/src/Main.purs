module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.String as Str
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes as ET
import DOM.HTML.Event.HashChangeEvent as HCE
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent) as DOM
import DOM.HTML.Types (windowToEventTarget) as DOM
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container as Container
import Router as Router
import CSS.Display (Display, block, displayNone)
import Halogen.HTML.CSS as CSS

import Network.RemoteData as RD
import Lib.MatrixApi (newApi, API, MatrixApis)

type State =
  { display :: Display }

initialState :: State
initialState =
  { display: displayNone }

data Query a
  = Initialize a
  | Finalize a
  | Response String a

type ChildQuery = Coproduct2 Router.Query
                             Container.Query

type ChildSlot = Either2 Unit Unit

ui :: forall e. H.Component HH.HTML Query Unit Void (MatrixApis e)
ui = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (MatrixApis e)
    render state =
      HH.div_
        [ HH.slot' CP.cp1 unit Router.ui unit absurd
        , HH.slot' CP.cp2 unit Container.ui unit absurd
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (MatrixApis e)
    eval (Initialize next) = do

      pure next
    eval (Finalize next) = do
      pure next
    eval (Response msg next) = do
      _ <- H.query' CP.cp2 unit $ H.action $ Container.RouteChange msg
      pure next

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer
  :: forall eff
   . CR.Producer DOM.HashChangeEvent (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
hashChangeProducer = CRA.produce \emit ->
  let
    emitter e =
      case runExcept (DOM.readHashChangeEvent (toForeign e)) of
        Left _ -> pure unit
        Right hce -> emit (Left hce)
  in
    liftEff $
      DOM.window
        >>= DOM.windowToEventTarget
        >>> DOM.addEventListener ET.hashchange (DOM.eventListener emitter) false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: forall eff
   . (Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer DOM.HashChangeEvent (Aff (HA.HalogenEffects eff)) Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  query $ H.action $ Response hash
  pure Nothing

main :: forall eff. Eff (HA.HalogenEffects (api :: API | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  packageList <- liftEff (newRef RD.NotAsked)
  matrixClient <- liftEff (newApi "/api" "/api")
  io <- runUI (H.hoist (\x -> runReaderT x { matrixClient, packageList }) ui) unit body
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
