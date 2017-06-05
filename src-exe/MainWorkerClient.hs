{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

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

import           PkgId
import           WorkerApi
import           WorkerApi.Client

queries :: Maybe PkgIdxTs -> ([CompilerID],[PkgId]) -> Manager -> BaseUrl -> ExceptT ServantError IO ()
queries idxts (ghcvers,pkgids) manager baseurl = do
    liftIO $ do
        print ghcvers
        print pkgids

    pretty =<< runClientM'' getWorkerInfo

    forM_ ghcvers $ \gv -> do
        forM_ pkgids $ \pid -> do
            CreateJobRes jobid <- runClientM'' $ createJob (CreateJobReq gv idxts pid)
            res1 <- runClientM'' $ getJobSolveInfo jobid
            pretty res1
            res2 <- runClientM'' $ getJobBuildDepsInfo jobid
            pretty res2
            res3 <- runClientM'' $ getJobBuildInfo jobid
            pretty res3
            runClientM'' $ destroyJob jobid

            -- get (CreateJobReq gv Nothing pid) manager baseurl

    return ()
  where
    pretty x = liftIO (putStrLn (show x))

    runClientM'' = runClientM' manager baseurl

main :: IO ()
main = do
    (idxtss:ghcverstr:args) <- getArgs

    let Just ghcver = mapM simpleParse (words ghcverstr)
        Just pkgs   = mapM simpleParse args
        idxts = read idxtss


    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
    res <- runExceptT (queries (Just idxts) (ghcver,pkgs) manager (BaseUrl Http "matrix-wrk6" 8001 "/api"))
    case res of
      Left (FailureResponse {..}) -> do
          putStrLn $ "Error:\n"
          print responseStatus
          print responseContentType
          putStrLn "----"
          BL.putStr responseBody
          putStrLn "\n----"
      Left err -> do
          putStrLn $ "Error:\n" ++ show err
      Right () -> putStrLn "DONE"



