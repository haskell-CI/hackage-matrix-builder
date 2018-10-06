{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude.Local

import           Control.Monad.Except (ExceptT (..), runExceptT)
-- import Data.Aeson
import           Network.HTTP.Client  (Manager, defaultManagerSettings,
                                       managerResponseTimeout, newManager,
                                       responseTimeoutNone)
import           Servant.Client
-- import           Text.Groom
import qualified Data.ByteString.Lazy as BL

import           Log
import           PkgId
import           WorkerApi
import           WorkerApi.Client

queries :: Maybe PkgIdxTs -> ([CompilerID],[PkgId]) -> Manager -> BaseUrl -> ExceptT ServantError IO ()
queries idxts (ghcvers,pkgids) manager baseurl = do
    logDebugShow ghcvers
    logDebugShow pkgids

    logDebugShow =<< runClientM'' getWorkerInfo

    forM_ ghcvers $ \gv -> do
        forM_ pkgids $ \pid -> do
            CreateJobRes jobid <- runClientM'' $ createJob (CreateJobReq gv idxts pid)
            res1 <- runClientM'' $ getJobSolveInfo jobid
            logDebugShow res1
            res2 <- runClientM'' $ getJobBuildDepsInfo jobid
            logDebugShow res2
            res3 <- runClientM'' $ getJobBuildInfo jobid
            logDebugShow res3
            runClientM'' $ destroyJob jobid

            -- get (CreateJobReq gv Nothing pid) manager baseurl

    return ()
  where
    runClientM'' = runClientM' manager baseurl

main :: IO ()
main = do
    getArgs >>= \case
      idxtss:ghcverstr:args -> go idxtss ghcverstr args
      _ -> do
        logError "usage: matrix-worker-client <idxstate> <ghcversion(s>) <pkgid1> [<pkgid2> [ ... ] ]"
        exitFailure

  where
    go idxtss ghcverstr args = do
      let Just ghcver = mapM simpleParse (words ghcverstr)
          Just pkgs   = mapM simpleParse args
          idxts = PkgIdxTs (read idxtss)
      manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
      res <- runExceptT (queries (Just idxts) (ghcver,pkgs) manager (BaseUrl Http "127.0.0.1" 8002 "/api"))
      case res of
        Left (FailureResponse {..}) -> do
            logDebugShow (responseStatus, responseContentType)
            BL.putStr responseBody
        Left err -> do
            logError (tshow err)
        Right () -> logInfo "DONE"



