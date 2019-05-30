-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module WorkerApi.Client where

import           Prelude.Local

import           Control.Monad.Except (ExceptT (..))
import           Servant.API
import           Servant.HttpStreams

import           PkgId
import           WorkerApi

runClientM' :: NFData a => BaseUrl -> ClientM a -> ExceptT ClientError IO a
runClientM' baseurl act = ExceptT $ withClientEnvIO baseurl (runClientM act)

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
