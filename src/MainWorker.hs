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

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Clock (UTCTime,getCurrentTime, NominalDiffTime, diffUTCTime)
import           Data.Version
import           Distribution.Simple.Program (findProgramVersion)
import           Distribution.Text
import           Distribution.Verbosity
import           GHC.Generics
import qualified Network.HTTP.Types as HTTP
import           Servant
import           Servant.Server.Internal.SnapShims (Application, applicationToSnap)
import           Snap.Core
import           Snap.Http.Server (defaultConfig)
import           Snap.Http.Server.Config (setPort)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.List as Streams
import           System.IO.Streams.Process
import           System.IO.Unsafe (unsafePerformIO)
import           System.Info (arch,os)
import           System.Process

import           IndexHelper
import           PkgId

data App = App
    { _appBootTime    :: POSIXTime
    , _appGhcVersions :: (Map.Map Version FilePath)

    , _appJobs        :: TVar (Map.Map JobId Job)
    }
-- makeLenses ''App


ghcExes :: [FilePath]
ghcExes =
    [ "/opt/ghc/8.0.1/bin/ghc"
    , "/opt/ghc/7.10.3/bin/ghc"
    , "/opt/ghc/7.8.4/bin/ghc"
    , "/opt/ghc/7.6.3/bin/ghc"
    , "/opt/ghc/7.4.2/bin/ghc"
    ]

cabalExe :: FilePath
cabalExe = "cabal"

workDir :: FilePath
workDir = "/tmp/matrix-worker"

main :: IO ()
main = do
    print =<< getPkgIndexTs

    createDirectoryIfMissing True workDir

    _appBootTime <- getPOSIXTime
    _appJobs <- newTVarIO mempty

    tmp <- forM ghcExes $ \x -> do
        Just v <- findProgramVersion "--numeric-version" id normal x
        return (v, x)

    let _appGhcVersions = Map.fromList tmp

    runWorker App {..} 8001

{-
doCabalUpdate :: IO ()
doCabalUpdate = do
    let cp = (proc "cabal" ["update"]) { std_in = NoStream }

    (s,ph) <- runProc "cabal" ["update", "-v"]
    print =<< Streams.atEOF s

    putStrLn "spawned..."

    -- print =<< Streams.read s

    rc <- waitForProcess ph

    putStrLn "done"

    print rc

    print =<< Streams.atEOF s
    out <- Streams.toList s
    print out

    print =<< Streams.atEOF s
    out2 <- Streams.toList s
    print out2
-}


-- merges stdout/stderr
runProc :: FilePath -> [String] -> IO (InputStream ByteString, ProcessHandle)
runProc exe args = do
    (rend, wend) <- createPipe

    let cp = (proc exe args) { std_in = NoStream, std_err = UseHandle wend, std_out = UseHandle wend }

    (Nothing, Nothing, Nothing, ph) <- createProcess cp

    sOutErr <- Streams.handleToInputStream rend >>=
               Streams.atEndOfInput (hClose rend) >>=
               Streams.lockingInputStream

    return (sOutErr,ph)

runProc' :: FilePath -> [T.Text] -> IO (T.Text, Int)
runProc' exe args = do
    -- TODO: use exception safe variant
    (s,ph) <- runProc exe (map T.unpack args)
    !rc <- ec2int <$> waitForProcess ph
    !bs <- T.decodeUtf8 <$> is2bs s
    pure (bs,rc)

data JobStep = JobStep
    { jsExitCode :: Int
    , jsLog      :: T.Text
    , jsStart    :: UTCTime
    , jsDuration :: NominalDiffTime
    } deriving Generic

instance ToJSON   JobStep where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobStep where { parseJSON = myParseJSON }

runStep :: FilePath -> [T.Text] -> IO JobStep
runStep exe args = do
    !jsStart <- getCurrentTime
    (jsLog,jsExitCode) <- runProc' exe args
    !jsDuration <- (`diffUTCTime` jsStart) <$> getCurrentTime
    pure $! JobStep{..}

is2lbs :: InputStream ByteString -> IO LBS.ByteString
is2lbs s = LBS.fromChunks <$> Streams.toList s

is2bs :: InputStream ByteString -> IO ByteString
is2bs s = mconcat <$> Streams.toList s


ec2int :: ExitCode -> Int
ec2int ExitSuccess = 0
ec2int (ExitFailure i) = i

{-



proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False,
                                delegate_ctlc = False,
                                detach_console = False,
                                create_new_console = False,
                                new_session = False,
                                child_group = Nothing,
                                child_user = Nothing }


-}

data WorkerInfo = WorkerInfo
    { wiUptime       :: Word
    , wiOsArch       :: (String,String)
    , wiGhcVersions  :: [Version]
    , wiJobQueueSize :: Word
    , wiIndexTs      :: Word
    -- TODO: os_release info
    } deriving (Generic, Show)

data CreateJobReq = CreateJobReq
    { cjrqGhcVersion :: Version
    , cjrqIndexTs    :: Maybe Word
    , cjrqPkgId      :: PkgId
    } deriving Generic

data CreateJobRes = CreateJobRes
    { cjrsJobId :: JobId
    } deriving Generic

instance ToJSON   WorkerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON WorkerInfo where { parseJSON = myParseJSON }

instance ToJSON   CreateJobReq where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobReq where { parseJSON = myParseJSON }

instance ToJSON   CreateJobRes where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobRes where { parseJSON = myParseJSON }

type JobsInfo = [JobId]

-- instance ToJSON JobsInfo
-- instance FromJSON JobsInfo

type WorkerApi m =
    -- GET /info
         "info" :> Get '[JSON] WorkerInfo
    -- /jobs
    :<|> "jobs" :> Get '[JSON] JobsInfo
    :<|> "jobs" :> ReqBody '[JSON] CreateJobReq :> PostCreated '[JSON] CreateJobRes -- TODO: use PostCreated
    :<|> "jobs" :> Capture "jid" JobId :> "plan" :> Get '[JSON] JobPlan
    :<|> "jobs" :> Capture "jid" JobId :> "report" :> Get '[JSON] JobReport
    :<|> "jobs" :> Capture "jid" JobId :> DeleteNoContent '[JSON] NoContent -- TODO: use DeleteNoContent


server :: Server (WorkerApi AppHandler) AppHandler
server = infoH :<|> jobsInfoH :<|> createJobH :<|> getJobPlanH :<|> getJobReportH :<|> destroyJobH
  where
    infoH :: AppHandler WorkerInfo
    infoH = do
        vs <- gets (Map.keys . _appGhcVersions)
        t0 <- gets _appBootTime
        t1 <- liftIO getPOSIXTime

        jobs <- gets _appJobs
        jobCnt <- (fromIntegral . Map.size) <$> liftIO (readTVarIO jobs)

        ts <- liftIO $ getPkgIndexTs

        pure (WorkerInfo (round $ t1-t0) (os,arch) vs jobCnt ts)

    jobsInfoH :: AppHandler JobsInfo
    jobsInfoH = do
        jobs <- gets _appJobs
        jids <- Map.keys <$> liftIO (readTVarIO jobs)
        pure jids

    getJobPlanH :: JobId -> AppHandler JobPlan
    getJobPlanH jid = lookupJob jid >>= \case
          Just j  -> setTimeout (60*60) >> liftIO (getJobPlan j)
          Nothing -> throwServantErr' err404

    getJobReportH :: JobId -> AppHandler JobReport
    getJobReportH jid = lookupJob jid >>= \case
          Just j  -> setTimeout (60*60) >> liftIO (getJobReport j)
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
              liftIO (forkIO (destroyJob j))
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


-- dummy
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

myToJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
myToJSON = genericToJSON (defaultOptions { fieldLabelModifier = labelMod })

myToEncoding :: (Generic a, GToEncoding (Rep a)) => a -> Encoding
myToEncoding = genericToEncoding (defaultOptions { fieldLabelModifier = labelMod })

myParseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
myParseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = labelMod })

labelMod :: String -> String
labelMod = camelTo2 '_' . dropWhile (not . isUpper)

-----

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

type JobId = Word

data Task a = TaskReady | TaskRunning (Async a)

data Job = Job
    { jobId    :: JobId
    , jobPkgId :: PkgId
    , jobGhcVer :: Version
    , jobGhcExe :: FilePath
    , jobIdxTs :: Word
    , jobState :: MVar JobSt

    -- each step depends on the previous one being completed
    , jobStepFetch     :: MVar (Task JobStep)
    , jobStepSolve     :: MVar (Task JobStep)
    , jobStepFetchDeps :: MVar (Task JobStep)
    , jobStepBuildDeps :: MVar (Task JobStep)
    , jobStepBuild     :: MVar (Task JobStep)
    }

data Step = StepFetch | StepSolve | StepFetchDeps | StepBuildDeps | StepBuild
          deriving (Ord,Eq,Enum,Bounded)

jobStep :: Step -> Job -> MVar (Task JobStep)
jobStep StepFetch     = jobStepFetch
jobStep StepSolve     = jobStepSolve
jobStep StepFetchDeps = jobStepFetchDeps
jobStep StepBuildDeps = jobStepBuildDeps
jobStep StepBuild     = jobStepBuild

runTask :: MVar (Task a) -> IO a -> IO a
runTask task go = do
    act <- modifyMVar task $ \case
        TaskReady -> do
            act' <- async go
            pure (TaskRunning act', act')
        TaskRunning act' ->
            pure (TaskRunning act', act')

    wait act


data JobSt = JobStCreated
           | JobStGetPlan (Async JobPlan)
           | JobStGetReport JobPlan (Async JobReport)
           | JobStDestroyed

jobFolder :: Job -> FilePath
jobFolder = (workDir </>) . show . jobId

-- | Creates new 'Job' in initial lifecycle state
--
-- In this initial state 'Job' can be garbage collected w/o needing to
-- finalize explicitly via 'destroyJob'
createNewJob :: (Version,FilePath) -> PkgId -> Word -> IO Job
createNewJob (jobGhcVer,jobGhcExe) jobPkgId jobIdxTs = do
    jobId <- round <$> getPOSIXTime
    jobState <- newMVar JobStCreated

    jobStepFetch     <- newMVar TaskReady
    jobStepSolve     <- newMVar TaskReady
    jobStepFetchDeps <- newMVar TaskReady
    jobStepBuildDeps <- newMVar TaskReady
    jobStepBuild     <- newMVar TaskReady

    pure (Job{..})

destroyJob :: Job -> IO ()
destroyJob (Job{..}) = do
    putStrLn ("destroyJob called for " ++ show jobId)

    modifyMVar_ jobState $ \s0 -> do
        case s0 of
          JobStCreated         -> pure ()
          JobStDestroyed       -> pure ()
          JobStGetPlan act     -> cancel act
          JobStGetReport _ act -> cancel act
        pure JobStDestroyed

    ex <- doesDirectoryExist wdir
    when ex $ removeDirectoryRecursive wdir
  where
    wdir = jobFolder Job{..}


getStep :: Step -> Job -> IO JobStep
getStep step (Job{..}) = do
    -- make sure previous step was performed
    unless (step == minBound) $ do
        _ <- getStep (pred step) (Job{..})
        pure ()

    runTask (jobStep step (Job{..})) (run step)
  where
    wdir     = jobFolder Job{..}
    pkgIdTxt = T.pack $ display jobPkgId

    ----------------------------------------------------------------------------

    run StepFetch = do
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

    run StepSolve = do
        ex <- doesDirectoryExist wdir
        when ex $ removeDirectoryRecursive wdir
        createDirectory wdir
        withCurrentDirectory wdir $ do
            runStep cabalExe ["get", pkgIdTxt]


    run StepFetchDeps = do



data JobPlan = JobPlan
    { jpDownload :: JobStep
    , jpSolve    :: Maybe JobStep
    } deriving Generic

instance ToJSON   JobPlan where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobPlan where { parseJSON = myParseJSON }

-- | Configures plan (if not done already), and waits for JobPlan
getJobPlan :: Job -> IO JobPlan
getJobPlan (Job{..}) = do
    !tmp <- modifyMVar jobState $ \s0 -> do
        case s0 of
          JobStCreated -> do
              act <- async go
              pure (JobStGetPlan act, Left act)
          JobStGetPlan act ->
              pure (s0, Left act)
          JobStGetReport plan _ ->
              pure (s0, Right plan)

    either wait pure tmp

  where
    pkgIdTxt = T.pack $ display jobPkgId

    go = do
        ex <- doesDirectoryExist wdir
        when ex $ removeDirectoryRecursive wdir
        createDirectory wdir

        withCurrentDirectory wdir $ do
            putStrLn "doing 'the' job-plan..."
            jpDownload <- runStep  cabalExe ["get", pkgIdTxt]

            jpSolve <- case jsExitCode jpDownload of
              0 -> do
                  T.writeFile "cabal.project" $ T.unlines
                      [ "packages: " <> pkgIdTxt <> "/"
                      , "with-compiler: " <> T.pack jobGhcExe
                      , "tests: false"
                      , "benchmarks: false"
                      , "jobs: 1"
                      , "build-log: logs/$libname.log"
                      , "-- index-state: " <> T.pack (show jobIdxTs)
                      ]

                  tmp <- runStep cabalExe ["new-build", "--dry"]
                  pure $! (Just $! tmp)

              _ -> pure Nothing

            putStrLn "...done the job-plan"
            pure (JobPlan {..})

    wdir = jobFolder Job{..}

data JobReport = JobReport
    { jrFetch :: JobStep
    , jrDeps  :: Maybe JobStep
    , jrBuild :: Maybe JobStep
    } deriving Generic

instance ToJSON   JobReport where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobReport where { parseJSON = myParseJSON }

decodeJobLog :: T.Text -> Maybe (Set.Set PkgId)
decodeJobLog s0 = do
    ("Resolving dependencies...":"In order, the following would be built (use -v for more details):":ls) <- pure $ T.lines s0
    pkgs <- mapM (decLine . T.unpack) ls
    pure (Set.fromList pkgs)
  where
    decLine (' ':'-':' ':rest) = simpleParse . takeWhile (/= ' ') $ rest
    decLine _                  = Nothing

getJobReport :: Job -> IO JobReport
getJobReport job@(Job {..}) = do
    plan <- getJobPlan job

    when ((jsExitCode <$> jpSolve plan) /= Just 0) $
        fail "getJobPlan: solving failed"

    let fetchPkgs = decodeJobLog . jsLog =<< jpSolve plan

    when (fetchPkgs == Nothing) $
        fail "failed to decode!"

    act <- modifyMVar jobState $ \s0 -> do
        case s0 of
          JobStGetPlan _ -> do
              act' <- async (go (maybe mempty id fetchPkgs))
              pure (JobStGetReport plan act', act')
          JobStGetReport _ act' ->
              pure (s0, act')

    wait act
  where
    go fetchPkgs = do
        withCurrentDirectory wdir $ do
            putStrLn "doing the report..."

            jrFetch <- runStep cabalExe ("fetch":"--no-dependencies":map (T.pack . display) (Set.toList fetchPkgs))

            jrDeps <- undefined

            jrBuild <- case jsExitCode jrFetch of
              0 -> do
                  !tmp <- runStep cabalExe ["new-build"]
                  pure $! Just $! tmp
              _ -> pure Nothing
            putStrLn "...done the report"
            pure (JobReport {..})

    wdir = jobFolder Job{..}
