module Main where

import Container as Container
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Halogen as H
import Halogen.Aff as HA
import Lib.MatrixApi as Api
import Lib.Types as T
import Network.RemoteData as RD
import Routing as Routing
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..), fromRight)
import Debug.Trace (traceAnyA)
import Halogen.VDom.Driver (runUI)
import Prelude (type (~>), Unit, bind, unit, pure, (<<<), ($))

main :: Eff (Api.MatrixEffects) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  packageList <- liftEff (newRef RD.NotAsked)
  matrixClient <- liftEff (Api.newApi "/api" "/api")
  io <- runUI (H.hoist (\x -> runReaderT x { matrixClient, packageList }) Container.ui) unit body
  CR.runProcess (matrixProducer CR.$$ matrixConsumer io.query)
  where
    matrixProducer :: CR.Producer T.PageRoute (Aff Api.MatrixEffects) Unit
    matrixProducer = CRA.produce \emit ->
      Routing.matches' Container.decodeURI Container.routing \_ -> emit <<< Left
    matrixConsumer :: (Container.Query ~> (Aff Api.MatrixEffects))
                   -> CR.Consumer T.PageRoute (Aff Api.MatrixEffects) Unit
    matrixConsumer query = do
      new <- CR.await
      _ <- H.lift (query (H.action (Container.RouteChange new)))
      matrixConsumer query
