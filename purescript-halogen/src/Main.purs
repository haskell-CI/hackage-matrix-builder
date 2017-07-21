module Main where

import Halogen as H
import Halogen.Aff as HA
import Network.RemoteData as RD
import Container as Container
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Monad.Reader (runReaderT)
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Tuple as T
import Halogen.VDom.Driver (runUI)
import Lib.MatrixApi as Api
import Routing as R
import Prelude (type (~>), Unit, bind, unit, pure, (>>=), (/=), (>>>), (<<<), ($))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
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
import Data.Argonaut as Arg


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

hashChangeConsumer
  :: forall eff
   . (Container.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer DOM.HashChangeEvent (Aff (HA.HalogenEffects eff)) Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
      -- route = R.matchWith (Str.drop 1 <<< Str.dropWhile (_ /= '#')) Container.routing $ HCE.newURL event
  _ <- query $ H.action $ Container.RouteChange (R.match Container.routing hash)
  pure Nothing

main :: forall eff. Eff (HA.HalogenEffects (api :: Api.API, ref :: REF | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  packageList <- liftEff (newRef RD.NotAsked)
  matrixClient <- liftEff (Api.newApi "/api" "/api")
  io <- runUI (H.hoist (\x -> runReaderT x { matrixClient, packageList }) Container.ui) unit body
  --T.Tuple old new <- R.matchesAff Container.routing
  --io.query $ H.action $ Container.RouteChange new
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
