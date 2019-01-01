-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module WorkerApi.Client where

import           Prelude.Local

import           Control.Monad.Except (ExceptT (..))
import           Network.HTTP.Client  (Manager)
import           Servant.API
import           Servant.Client

import           PkgId
import           WorkerApi

runClientM' :: Manager -> BaseUrl -> ClientM a -> ExceptT ServantError IO a
runClientM' manager' baseurl act = ExceptT (runClientM act (ClientEnv manager' baseurl))

getWorkerInfo       ::                 ClientM WorkerInfo
getJobsInfo         ::                 ClientM [JobId]
createJob           :: CreateJobReq -> ClientM CreateJobRes
getJobSolveInfo     :: JobId ->        ClientM JobSolve
getJobBuildDepsInfo :: JobId ->        ClientM JobBuildDeps
getJobBuildInfo     :: JobId ->        ClientM JobBuild
destroyJob          :: JobId ->        ClientM NoContent
listCompilers       ::                 ClientM [CompilerID]
listPkgDbGlobal     :: CompilerID ->   ClientM [GPkgInfo]
listPkgDbStore      :: CompilerID ->   ClientM [SPkgInfo]
destroyPkgDbStore   :: CompilerID ->   ClientM NoContent

getWorkerInfo          :<|>
   getJobsInfo         :<|>
   createJob           :<|>
   getJobSolveInfo     :<|>
   getJobBuildDepsInfo :<|>
   getJobBuildInfo     :<|>
   destroyJob          :<|>

   listCompilers       :<|>
   listPkgDbGlobal     :<|>
   listPkgDbStore      :<|>
   destroyPkgDbStore
     = client workerApi

workerApi :: Proxy (WorkerApi ())
workerApi = Proxy
