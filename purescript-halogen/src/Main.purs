module Main where

import Halogen as H
import Halogen.Aff as HA
import Network.RemoteData as RD
import Container as Container
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Monad.Reader (runReaderT)
import Data.Tuple as T
import Halogen.VDom.Driver (runUI)
import Lib.MatrixApi as Api
import Prelude (Unit, bind, unit, ($))
import Routing as R

main :: forall eff. Eff (HA.HalogenEffects (api :: Api.API, ref :: REF | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  packageList <- liftEff (newRef RD.NotAsked)
  matrixClient <- liftEff (Api.newApi "/api" "/api")
  io <- runUI (H.hoist (\x -> runReaderT x { matrixClient, packageList }) Container.ui) unit body
  T.Tuple old new <- R.matchesAff Container.routing
  io.query $ H.action $ Container.RouteChange new
