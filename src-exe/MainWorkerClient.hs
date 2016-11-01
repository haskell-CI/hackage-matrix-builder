{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude.Local

import Control.Monad.Except (ExceptT, runExceptT)
-- import Data.Aeson
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutNone)

import Servant.Client
import Text.Groom

import WorkerApi
import WorkerApi.Client
import PkgId
import qualified Data.ByteString.Lazy as BL

queries :: Maybe PkgIdxTs -> ([CompilerID],[PkgId]) -> Manager -> BaseUrl -> ExceptT ServantError IO ()
queries idxts (ghcvers,pkgids) manager baseurl = do
    liftIO $ do
        print ghcvers
        print pkgids

    pretty =<< getWorkerInfo manager baseurl

    forM_ ghcvers $ \gv -> do
        forM_ pkgids $ \pid -> do
            CreateJobRes jobid <- createJob (CreateJobReq gv idxts pid) manager baseurl
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
    (idxtss:ghcverstr:args) <- getArgs

    let Just ghcver = mapM simpleParse (words ghcverstr)
        Just pkgs   = mapM simpleParse args
        idxts = read idxtss


    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
    res <- runExceptT (queries (Just idxts) (ghcver,pkgs) manager (BaseUrl Http "matrix-wrk1" 8001 "/api"))
    case res of
      Left (FailureResponse {..}) -> do
          putStrLn $ "Error:\n"
          print responseStatus
          print responseContentType
          putStrLn "----"
          BL.putStr responseBody
          putStrLn "\n----"
      Left err -> do
          putStrLn $ "Error:\n" ++ groom err
      Right () -> putStrLn "DONE"



