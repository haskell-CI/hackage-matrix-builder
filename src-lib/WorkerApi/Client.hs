module WorkerApi.Client where

import Prelude.Local

-- import Control.Monad.Except (ExceptT, runExceptT)
-- import Data.Aeson
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

import WorkerApi
import PkgId


getWorkerInfo       ::                 Manager -> BaseUrl -> ClientM WorkerInfo
getJobsInfo         ::                 Manager -> BaseUrl -> ClientM [JobId]
createJob           :: CreateJobReq -> Manager -> BaseUrl -> ClientM CreateJobRes
getJobSolveInfo     :: JobId ->        Manager -> BaseUrl -> ClientM JobSolve
getJobBuildDepsInfo :: JobId ->        Manager -> BaseUrl -> ClientM JobBuildDeps
getJobBuildInfo     :: JobId ->        Manager -> BaseUrl -> ClientM JobBuild
destroyJob          :: JobId ->        Manager -> BaseUrl -> ClientM NoContent
listCompilers       ::                 Manager -> BaseUrl -> ClientM [CompilerID]
listPkgDbGlobal     :: CompilerID ->   Manager -> BaseUrl -> ClientM [GPkgInfo]
listPkgDbStore      :: CompilerID ->   Manager -> BaseUrl -> ClientM [SPkgInfo]
destroyPkgDbStore   :: CompilerID ->   Manager -> BaseUrl -> ClientM NoContent

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
