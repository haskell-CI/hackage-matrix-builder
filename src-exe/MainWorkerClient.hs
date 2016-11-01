{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude.Local

import Control.Monad.Except (ExceptT, runExceptT)
-- import Data.Aeson
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutNone)
import Servant.API
import Servant.Client
import Text.Groom

import WorkerApi
import PkgId

workerApi :: Proxy (WorkerApi ())
workerApi = Proxy


getWorkerInfo       ::                 Manager -> BaseUrl -> ClientM WorkerInfo
getJobsInfo         ::                 Manager -> BaseUrl -> ClientM [JobId]
createJob           :: CreateJobReq -> Manager -> BaseUrl -> ClientM CreateJobRes
getJobSolveInfo     :: JobId ->        Manager -> BaseUrl -> ClientM JobSolve
getJobBuildDepsInfo :: JobId ->        Manager -> BaseUrl -> ClientM JobBuildDeps
getJobBuildInfo     :: JobId ->        Manager -> BaseUrl -> ClientM JobBuild
destroyJob          :: JobId ->        Manager -> BaseUrl -> ClientM NoContent

getWorkerInfo :<|>
   getJobsInfo :<|>

   createJob           :<|>
   getJobSolveInfo     :<|>
   getJobBuildDepsInfo :<|>
   getJobBuildInfo     :<|>
   destroyJob = client workerApi

queries :: ([Ver],[PkgId]) -> Manager -> BaseUrl -> ExceptT ServantError IO ()
queries (ghcvers,pkgids) manager baseurl = do
    liftIO $ do
        print ghcvers
        print pkgids

    pretty =<< getWorkerInfo manager baseurl

    forM_ ghcvers $ \gv -> do
        forM_ pkgids $ \pid -> do
            CreateJobRes jobid <- createJob (CreateJobReq gv Nothing pid) manager baseurl
            res1 <- getJobSolveInfo jobid manager baseurl
            pretty res1
            res2 <- getJobBuildDepsInfo jobid manager baseurl
            pretty res2
            res3 <- getJobBuildInfo jobid manager baseurl
            pretty res3
            destroyJob jobid manager baseurl


            -- get (CreateJobReq gv Nothing pid) manager baseurl

    return ()
  where
    pretty x = liftIO (putStrLn (groom x))

main :: IO ()
main = do
    (ghcverstr:args) <- getArgs

    let Just ghcver = mapM simpleParse (words ghcverstr)
        Just pkgs   = mapM simpleParse args

    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
    res <- runExceptT (queries (ghcver,pkgs) manager (BaseUrl Http "localhost" 8001 "/api"))
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right () -> putStrLn "DONE"



