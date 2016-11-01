{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Prelude.Local

import           Control.Concurrent
import           Control.Concurrent.STM
-- import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Distribution.Simple.Program (findProgramVersion)
import           Distribution.Verbosity
import qualified Network.HTTP.Types as HTTP
import           Servant
import           Servant.Server.Internal.SnapShims (Application, applicationToSnap)
import           Snap.Core
import           Snap.Http.Server (defaultConfig)
import           Snap.Http.Server.Config (setPort)
import           Snap.Snaplet
import qualified System.IO.Streams as Streams
-- import qualified System.IO.Streams.List as Streams
-- import           System.IO.Streams.Process
-- import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Info
-- import           System.Process


import           Data.Profunctor.Product -- (p2, p3)
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import qualified Opaleye.Internal.Unpackspec as U


import           IndexHelper
import           Job
import           PkgId
import           System.Exit
import           WorkerApi

-- data App = App
--     { _appBootTime    :: POSIXTime
--     , _appJobs        :: TVar (Map.Map JobId Job)
--     }
-- makeLenses ''App

cabalExe :: FilePath
cabalExe = "cabal"

main :: IO ()
main = do
    putStrLn "start"
    dbconn <- PGS.connect (PGS.ConnectInfo "" 0 "" "" "matrix")


    xs <- runQuery dbconn (queryTable pkgIndexTable)

    print (take 1 (xs :: [(Text, Text, Int, Int, Text)]))

    tmp2 <- readIndexTuples indexTar


    -- print (length tmp2)
    forM_ tmp2 $ \PkgIdxTuple{..} -> do
      print ()
      let rev = fromIntegral pitRev :: Int
          ver = maybe "" display $ pitVer
      _ <- runInsert dbconn pkgIndexTable $ constant (pitName, ver, rev, (fromIntegral pitTime) :: Int, pitOwner)
      pure ()

    pure ()
    -- _ <- exitSuccess

    -- _appBootTime <- getPOSIXTime
    -- _appJobs <- newTVarIO mempty

    -- runWorker App {..} 8001


is2lbs :: InputStream ByteString -> IO LBS.ByteString
is2lbs s = LBS.fromChunks <$> Streams.toList s



printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "???" id . showSqlForPostgres


pkgIndexTable :: Table (Column PGText, Column PGText, Column PGInt4, Column PGInt4, Column PGText)
                       (Column PGText, Column PGText, Column PGInt4, Column PGInt4, Column PGText)
pkgIndexTable = Table "pkgindex" (p5 ( required "pname"
                                     , required "pver"
                                     , required "prev"
                                     , required "ptime"
                                     , required "powner" ))



{-
server :: Server (WorkerApi AppHandler) AppHandler
server = infoH :<|> jobsInfoH :<|> createJobH :<|> getJobSolveH :<|> getJobBuildDepsH :<|> getJobBuildH :<|> destroyJobH
  where
    infoH :: AppHandler WorkerInfo
    infoH = do
        t0 <- gets _appBootTime
        t1 <- liftIO getPOSIXTime

        jobs <- gets _appJobs
        jobCnt <- (fromIntegral . Map.size) <$> liftIO (readTVarIO jobs)

        ts <- liftIO $ getPkgIndexTs

        pure (WorkerInfo (round $ t1-t0) (System.Info.os,System.Info.arch) vs jobCnt ts)

    jobsInfoH :: AppHandler JobsInfo
    jobsInfoH = do
        jobs <- gets _appJobs
        jids <- Map.keys <$> liftIO (readTVarIO jobs)
        pure jids


    getJobSolveH :: JobId -> AppHandler JobSolve
    getJobSolveH jid = lookupJob jid >>= \case
          Just j  -> setTimeout (60*60) >> liftIO (getJobSolve j)
          Nothing -> throwServantErr' err404

    getJobBuildDepsH :: JobId -> AppHandler JobBuildDeps
    getJobBuildDepsH jid = lookupJob jid >>= \case
          Just j  -> setTimeout (60*60) >> liftIO (getJobBuildDeps j)
          Nothing -> throwServantErr' err404

    getJobBuildH :: JobId -> AppHandler JobBuild
    getJobBuildH jid = lookupJob jid >>= \case
          Just j  -> setTimeout (60*60) >> liftIO (getJobBuild j)
          Nothing -> throwServantErr' err404

    destroyJobH :: JobId -> AppHandler NoContent
    destroyJobH jid = do
        tvjobs <- gets _appJobs

        mjob <- liftIO $ atomically $ do
            jobs <- readTVar tvjobs
            let (mjob', jobs') = mapExtract jid jobs
            unless (isNothing mjob') $
                writeTVar tvjobs jobs'
            return mjob'

        case mjob of
          Just j -> do
              _ <- liftIO (forkIO (destroyJob j))
              pure NoContent
          Nothing -> throwServantErr0 err404

    createJobH :: CreateJobReq -> AppHandler CreateJobRes
    createJobH CreateJobReq {..} = do
        mGhcExe <- gets (Map.lookup cjrqGhcVersion . _appGhcVersions)
        tvjobs  <- gets _appJobs

        case mGhcExe of
          Nothing -> throwServantErr' err400

          Just ghcExe -> do
              itm <- liftIO $ readPkgIndex

              its <- case cjrqIndexTs of
                Nothing -> pure $ fromIntegral $ fst (IntMap.findMax itm)
                Just ts0 -> if IntMap.member (fromIntegral ts0) itm then pure ts0 else (throwServantErr' err400) --FIXME

              let Just itm' = IntMap.lookup (fromIntegral its) itm
                  exists = Set.member cjrqPkgId itm'

              unless exists $
                  throwServantErr' err400

              newJob <- liftIO $ createNewJob (cjrqGhcVersion,ghcExe) cjrqPkgId its
              let jid = jobId newJob

              mjob <- liftIO $ atomically $ do
                  jobs <- readTVar tvjobs
                  if (Map.size jobs >= 1) || (jid `Map.member` jobs)
                      then return Nothing
                      else do
                         writeTVar tvjobs (Map.insert jid newJob jobs)
                         pure (Just newJob)

              case mjob of
                Just _ -> pure (CreateJobRes jid)
                Nothing -> throwServantErr' err503


    lookupJob jid = do
        tvjobs <- gets _appJobs
        Map.lookup jid <$> liftIO (readTVarIO tvjobs)

type AppHandler = Handler App App

workerApi :: Proxy (WorkerApi AppHandler)
workerApi = Proxy

runWorker :: App -> Int -> IO ()
runWorker !app port =
    serveSnaplet (setPort port defaultConfig) initApp
  where
    initApp :: SnapletInit App App
    initApp = makeSnaplet "matrix-worker" "Matrix CI worker" Nothing $ do
        addRoutes [("api", applicationToSnap test)]
        return app

    test :: Application AppHandler
    test = serve workerApi server

----------------------------------------------------------------------------


mapExtract :: Ord k => k -> Map.Map k a -> (Maybe a, Map.Map k a)
mapExtract = Map.updateLookupWithKey (\_ _ -> Nothing)

-----

-- throwHttpErr :: MonadSnap m => m b
-- throwHttpErr = throwServantErr' err404

throwServantErr' :: MonadSnap m => ServantErr -> m b
throwServantErr' err =
    throwServantErr0 (err { errBody = "null", errHeaders = (HTTP.hContentType, "application/json") : errHeaders err })

throwServantErr0 :: MonadSnap m => ServantErr -> m b
throwServantErr0 ServantErr{..} = do
    modifyResponse $ setResponseStatus errHTTPCode (BS.pack errReasonPhrase)
    modifyResponse $ setHeaders errHeaders
    writeLBS errBody
    Snap.Core.getResponse >>= finishWith
  where
    setHeaders :: [HTTP.Header] -> Response -> Response
    setHeaders hs r = foldl' (\r' (h, h') -> Snap.Core.addHeader h h' r') r hs

----------------------------------------------------------------------------

data Job = Job
    { jobId     :: JobId
    , jobPkgId  :: PkgId
    , jobGhcVer :: Ver
    , jobGhcExe :: FilePath
    , jobIdxTs  :: Word

    -- each step depends on the previous one being completed
    , jobStepFetch     :: MVar (Task JobStep)
    , jobStepSolve     :: MVar (Task JobStep)
    , jobStepFetchDeps :: MVar (Task JobStep)
    , jobStepBuildDeps :: MVar (Task JobStep)
    , jobStepBuild     :: MVar (Task JobStep)
    }

data Step = StepFetch
          | StepSolve
          | StepFetchDeps
          | StepBuildDeps
          | StepBuild
          deriving (Ord,Eq,Enum,Bounded,Show)

jobStep :: Step -> Job -> MVar (Task JobStep)
jobStep StepFetch     = jobStepFetch
jobStep StepSolve     = jobStepSolve
jobStep StepFetchDeps = jobStepFetchDeps
jobStep StepBuildDeps = jobStepBuildDeps
jobStep StepBuild     = jobStepBuild

jobFolder :: Job -> FilePath
jobFolder = (workDir </>) . show . jobId

-- | Creates new 'Job' in initial lifecycle state
--
-- In this initial state 'Job' can be garbage collected w/o needing to
-- finalize explicitly via 'destroyJob'
createNewJob :: (Ver,FilePath) -> PkgId -> Word -> IO Job
createNewJob (jobGhcVer,jobGhcExe) jobPkgId jobIdxTs = do
    jobId <- round <$> getPOSIXTime

    jobStepFetch     <- newTask
    jobStepSolve     <- newTask
    jobStepFetchDeps <- newTask
    jobStepBuildDeps <- newTask
    jobStepBuild     <- newTask

    pure (Job{..})

destroyJob :: Job -> IO ()
destroyJob (Job{..}) = do
    putStrLn ("destroyJob called for " ++ show jobId)

    forM_ [minBound..maxBound] $ \step -> do
        cancelTask (jobStep step (Job{..}))

    ex <- doesDirectoryExist wdir
    when ex $ removeDirectoryRecursive wdir
  where
    wdir = jobFolder Job{..}

-- returns 'Nothing' if not run because preq failed
getStep :: Step -> Job -> IO (Maybe JobStep)
getStep step (Job{..}) = do
    -- make sure previous step was performed succesfully
    prevOk <- if step == minBound
        then pure True
        else do
          prevStep <- getStep (pred step) (Job{..})
          pure (maybe (-1) jsExitCode prevStep == 0)

    if prevOk
        then Just <$> runTask (jobStep step (Job{..})) (run' step)
        else pure Nothing
  where
    wdir     = jobFolder Job{..}
    pkgIdTxt = T.pack $ display jobPkgId

    run' step' = do
        putStrLn (concat ["[",show jobId, "] starting ", show step])
        res <- run step'
        putStrLn (concat ["[",show jobId, "] finished ", show step, " rc=", show (jsExitCode res)])
        pure res

    ----------------------------------------------------------------------------
    -- job steps

    run StepFetch = do
        ex <- doesDirectoryExist wdir
        when ex $ removeDirectoryRecursive wdir
        createDirectory wdir
        withCurrentDirectory wdir $ do
            runStep cabalExe ["get", pkgIdTxt]

    run StepSolve = do
        withCurrentDirectory wdir $ do
            T.writeFile "cabal.project" $ T.unlines
                [ "packages: " <> pkgIdTxt <> "/"
                , "with-compiler: " <> T.pack jobGhcExe
                , "tests: false"
                , "benchmarks: false"
                , "jobs: 1"
                , "build-log: logs/$libname.log"
                , "-- index-state: " <> T.pack (show jobIdxTs)
                ]

            runStep cabalExe ["new-build", "--dry"]

    run StepFetchDeps = do
        msolve <- getStep StepSolve (Job{..})
        let mfetchPkgs = decodeJobLog . jsLog =<< msolve

        case mfetchPkgs of
          Nothing -> fail "failed to decode!"

          Just fetchPkgs -> withCurrentDirectory wdir $
              runStep cabalExe ("fetch":"--no-dependencies":map
                                 (T.pack . display)
                                 (Set.toList fetchPkgs))

    run StepBuildDeps = do
        withCurrentDirectory wdir $ do
            runStep cabalExe ["new-build", "--only-dependencies", pkgIdTxt]

    run StepBuild = do
        withCurrentDirectory wdir $ do
            runStep cabalExe ["new-build"]


decodeJobLog :: T.Text -> Maybe (Set.Set PkgId)
decodeJobLog s0 = do
    ("Resolving dependencies...":"In order, the following would be built (use -v for more details):":ls) <- pure $ T.lines s0
    pkgs <- mapM (decLine . T.unpack) ls
    pure (Set.fromList pkgs)
  where
    decLine (' ':'-':' ':rest) = simpleParse . takeWhile (/= ' ') $ rest
    decLine _                  = Nothing


-- | Configures plan (if not done already), and waits for JobSolve
getJobSolve :: Job -> IO JobSolve
getJobSolve (Job{..}) = do
    jpFetch  <- getStep StepFetch (Job{..})
    jpSolve  <- getStep StepSolve (Job{..})

    pure (JobSolve{..})


getJobBuildDeps :: Job -> IO JobBuildDeps
getJobBuildDeps (Job{..}) = do
    jrFetchDeps <- getStep StepFetchDeps (Job{..})
    jrBuildDeps <- getStep StepBuildDeps (Job{..})

    pure (JobBuildDeps{..})


getJobBuild :: Job -> IO JobBuild
getJobBuild (Job{..}) = do
    jrBuild     <- getStep StepBuild     (Job{..})

    pure (JobBuild{..})

-}
