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

module Main where

import           Prelude.Local

import qualified Data.List.NonEmpty as NonEmpty
import           Control.Concurrent
import           Control.Concurrent.STM
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Aeson as J
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Distribution.Verbosity
import qualified Network.HTTP.Types as HTTP
import           Servant
import           Snap.Core
import           Snap.Http.Server (defaultConfig)
import qualified Snap.Http.Server.Config as Config
import           Snap.Snaplet
import qualified System.IO.Streams as Streams
-- import qualified System.IO.Streams.List as Streams
-- import           System.IO.Streams.Process
-- import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Info
-- import           System.Exit
-- import           System.Process

import System.Posix.Files
import System.Posix.User

import           Distribution.Simple.GHC as GHC
import           Distribution.Simple.Program
import           Distribution.Simple.Compiler
import           Distribution.InstalledPackageInfo
import           Distribution.Simple.PackageIndex
import qualified Config as Cfg
import qualified Config.Lens as Cfg

import           Control.Concurrent.ReadWriteLock        ( RWLock )
import qualified Control.Concurrent.ReadWriteLock as RWL

import           Data.Ratio
import           IndexHelper
import           Job
import           PkgId
import           PlanJson
import           WorkerApi

data App = App
    { _appBootTime    :: POSIXTime
    , _appGhcVersions :: (Map.Map CompilerID (FilePath,ProgramDb,[GPkgInfo]))

    , _appJobs        :: TVar (Map.Map JobId Job)

    -- TODO/FIXME: make locks this per-compilerid;
    -- jobs aquire write-lock during build-phases
    , _appStoreBuildLock :: RWLock
    -- jobs aquire read-lock; pkg deletion aquires write-lock
    , _appStoreDelLock :: RWLock
    , _appWorkDir     :: FilePath
    }
-- makeLenses ''App

-- dirty hack:
{-# NOINLINE cabalExe #-}
cabalExe :: FilePath
cabalExe = unsafePerformIO (readMVar cabalExeRef)

{-# NOINLINE cabalExeRef #-}
cabalExeRef :: MVar FilePath
cabalExeRef = unsafePerformIO newEmptyMVar

data WorkerConf = WorkerConf
    { wcExes     :: [FilePath]
    , wcPort     :: Word16
    , wcCabalExe :: FilePath
    , wcWorkDir  :: FilePath
    } deriving Show

readConfig :: FilePath -> IO WorkerConf
readConfig fn = do
    Right cfg <- Cfg.parse <$> T.readFile fn

    let wcExes = T.unpack <$> (cfg ^.. Cfg.key "compiler-exe" . Cfg.list . traversed . Cfg.text)
        Just wcPort     = fromIntegral <$> (cfg ^? Cfg.key "port" . Cfg.number)
        Just wcWorkDir  = T.unpack     <$> (cfg ^? Cfg.key "workdir" . Cfg.text)
        Just wcCabalExe = T.unpack     <$> (cfg ^? Cfg.key "cabal-exe" . Cfg.text)

    pure WorkerConf{..}

main :: IO ()
main = do
    _appBootTime <- getPOSIXTime
    _appJobs <- newTVarIO mempty

    WorkerConf{..} <- getArgs >>= \case
        [fn] -> readConfig fn
        _    -> die "usage: matrix-worker <configfile>"

    putMVar cabalExeRef wcCabalExe

    print WorkerConf{..}

    print =<< runProc' cabalExe ["--version"]

    tmp <- forM wcExes $ \x -> do
        -- print ghcExes
        (com, _pla, pdb) <- configure normal (Just x) Nothing defaultProgramDb

        let Just hcid = compilerIDFromCompilerId $ compilerId com
        glob_pkgs <- GHC.getPackageDBContents normal GlobalPackageDB pdb
        let gpkgs = [ GPkgInfo (fromMaybe undefined $ pkgIdFromPackageIdentifier $ sourcePackageId p)
                               (unitIDFromUnitId $ installedUnitId p)
                               (Set.fromList $ map unitIDFromUnitId $ depends p)
                    | p <- allPackages glob_pkgs ]

        return (hcid, (x,pdb,gpkgs))

    let _appGhcVersions = Map.fromList tmp
        _appWorkDir = wcWorkDir

    _appStoreDelLock <- RWL.new
    _appStoreBuildLock <- RWL.new

    --- () <- exitSuccess

    its <- getPkgIndexTs
    putStrLn ("index-state at startup: " ++ show its)

    createDirectoryIfMissing True wcWorkDir
    runWorker App {..} wcPort



is2lbs :: InputStream ByteString -> IO LBS.ByteString
is2lbs s = LBS.fromChunks <$> Streams.toList s




server :: Server (WorkerApi AppHandler) AppHandler
server =
    infoH :<|> jobsInfoH :<|> createJobH :<|> getJobSolveH :<|> getJobBuildDepsH :<|> getJobBuildH :<|> destroyJobH :<|> listCompilers :<|> listPkgDbGlobal :<|> listPkgDbStore :<|> destroyPkgDbStoreH
  where
    infoH :: AppHandler WorkerInfo
    infoH = do
        vs <- gets (Map.keys . _appGhcVersions)
        t0 <- gets _appBootTime
        t1 <- liftIO getPOSIXTime

        jobs <- gets _appJobs
        jobCnt <- (fromIntegral . Map.size) <$> liftIO (readTVarIO jobs)

        ts <- liftIO $ getPkgIndexTs

        pure (WorkerInfo (round $ t1-t0) os_arch vs jobCnt ts)
      where
        os_arch = (T.pack System.Info.os,T.pack System.Info.arch)

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
        dellock <- gets _appStoreDelLock

        mjob <- liftIO $ atomically $ do
            jobs <- readTVar tvjobs
            let (mjob', jobs') = mapExtract jid jobs
            unless (isNothing mjob') $
                writeTVar tvjobs jobs'
            return mjob'

        case mjob of
          Just j -> do
              liftIO $ cleanupTmp
              liftIO $ RWL.releaseRead dellock
              _ <- liftIO (forkIO (destroyJob j))
              pure NoContent
          Nothing -> throwServantErr0 err404

    createJobH :: CreateJobReq -> AppHandler CreateJobRes
    createJobH CreateJobReq {..} = do
        mGhcExe <- gets (Map.lookup cjrqGhcVersion . _appGhcVersions)
        tvjobs  <- gets _appJobs
        wdir    <- gets _appWorkDir
        buildlock <- gets _appStoreBuildLock
        dellock <- gets _appStoreDelLock

        liftIO $ print (CreateJobReq{..})

        case mGhcExe of
          Nothing -> throwServantErr' err400

          Just (ghcExe,_,_) -> do
              itm0 <- liftIO $ readPkgIndex
              let headts0 = fromIntegral $ fst (IntMap.findMax itm0)

              (itm,its) <- case cjrqIndexTs of
                Nothing -> pure (itm0,headts0)
                Just ts0 -> do
                    when (ts0 > headts0) $ do
                        -- TODO: use runStep instead
                        res <- liftIO $ runProc' cabalExe [ "update"
                                                          , "--verbose=normal+nowrap+timestamp"]
                        liftIO $ print res

                    itm <- liftIO $ readPkgIndex

                    if IntMap.member (fromIntegral ts0) itm
                            then pure (itm,ts0)
                            else (throwServantErr' err400) --FIXME

              let Just itm' = IntMap.lookup its itm
                  exists = Set.member cjrqPkgId itm'

              unless exists $
                  throwServantErr' err400

              newJob <- liftIO $ createNewJob buildlock wdir (cjrqGhcVersion,ghcExe) cjrqPkgId its
              let jid = jobId newJob

              mjob <- liftIO $ atomically $ do
                  jobs <- readTVar tvjobs
                  if (Map.size jobs >= 1) || (jid `Map.member` jobs)
                      then return Nothing
                      else do
                         writeTVar tvjobs (Map.insert jid newJob jobs)
                         pure (Just newJob)

              case mjob of
                Just _ -> do
                    liftIO $ RWL.acquireRead dellock
                    pure (CreateJobRes jid)
                Nothing -> throwServantErr' err503


    lookupJob jid = do
        tvjobs <- gets _appJobs
        Map.lookup jid <$> liftIO (readTVarIO tvjobs)

    listCompilers = do
        hcs <- gets _appGhcVersions
        pure $ Map.keys hcs

    listPkgDbGlobal :: CompilerID -> AppHandler [GPkgInfo]
    listPkgDbGlobal cid = do
        hcs <- gets _appGhcVersions

        liftIO $ print (cid, Map.keys hcs)

        (_,_,gpkgs) <- maybe (throwServantErr' err404) pure (Map.lookup cid hcs)

        pure gpkgs

    -- FIXME: must not occur while build steps are running
    listPkgDbStore :: CompilerID -> AppHandler [SPkgInfo]
    listPkgDbStore cid = do
        hcs <- gets _appGhcVersions
        (_,pdb,_) <- maybe (throwServantErr' err404) pure (Map.lookup cid hcs)

        buildlock <- gets _appStoreBuildLock
        dellock <- gets _appStoreDelLock

        res <- liftIO $ RWL.tryWithRead dellock $ RWL.tryWithRead buildlock $ do
            pdbfn <- getAppUserDataDirectory ("cabal/store/" ++ display cid ++ "/package.db")
            ex <- doesDirectoryExist pdbfn
            if ex
              then do
                glob_pkgs <- GHC.getPackageDBContents normal (SpecificPackageDB pdbfn) pdb
                pure [ SPkgInfo (fromMaybe undefined $ pkgIdFromPackageIdentifier $ sourcePackageId p)
                       (unitIDFromUnitId $ installedUnitId p)
                       (Set.fromList $ map unitIDFromUnitId $ depends p)
                     | p <- allPackages glob_pkgs ]
              else
                pure []

        case res of
          Just (Just v) -> pure v
          _ -> throwServantErr' err503

    -- FIXME: invariant: must not occur while jobs exist
    destroyPkgDbStoreH :: CompilerID -> AppHandler NoContent
    destroyPkgDbStoreH cid = do
        hcs <- gets _appGhcVersions
        dellock <- gets _appStoreDelLock

        unless (Map.member cid hcs) $
            throwServantErr' err404

        res <- liftIO $ RWL.tryWithWrite dellock $ do
            pdbfn <- getAppUserDataDirectory ("cabal/store/" ++ display cid)
            ex <- doesDirectoryExist pdbfn
            when ex $ removeDirectoryRecursive pdbfn

        case res of
          Just () -> pure NoContent
          Nothing -> throwServantErr' err503

type AppHandler = Handler App App

workerApi :: Proxy (WorkerApi AppHandler)
workerApi = Proxy

runWorker :: App -> Word16 -> IO ()
runWorker !app port = do
    cwd <- getCurrentDirectory

    let cfg = Config.setAccessLog (Config.ConfigFileLog $ cwd </> "log" </> "access.log") $
              Config.setErrorLog  (Config.ConfigFileLog $ cwd </> "log" </> "error.log") $
              Config.setPort (fromIntegral port) $
              Config.setDefaultTimeout 1800 $
              defaultConfig

    serveSnaplet cfg initApp
  where
    initApp :: SnapletInit App App
    initApp = makeSnaplet "matrix-worker" "Matrix CI worker" Nothing $ do
        addRoutes [("api", test)]
        return app

    test :: AppHandler ()
    test = serveSnap workerApi server

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
    , jobGhcVer :: CompilerID
    , jobGhcExe :: FilePath
    , jobIdxTs  :: PkgIdxTs
    , jobFolder :: FilePath
    , jobBuildLock :: RWLock

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

-- | Creates new 'Job' in initial lifecycle state
--
-- In this initial state 'Job' can be garbage collected w/o needing to
-- finalize explicitly via 'destroyJob'
createNewJob :: RWLock -> FilePath -> (CompilerID,FilePath) -> PkgId -> PkgIdxTs -> IO Job
createNewJob jobBuildLock wdir (jobGhcVer,jobGhcExe) jobPkgId jobIdxTs = do
    jobId <- round <$> getPOSIXTime

    jobStepFetch     <- newTask
    jobStepSolve     <- newTask
    jobStepFetchDeps <- newTask
    jobStepBuildDeps <- newTask
    jobStepBuild     <- newTask

    let jobFolder = wdir </> show jobId

    pure (Job{..})

destroyJob :: Job -> IO ()
destroyJob (Job{..}) = do
    putStrLn ("destroyJob called for " ++ show jobId)

    forM_ [minBound..maxBound] $ \step -> do
        cancelTask (jobStep step (Job{..}))

    ex <- doesDirectoryExist wdir
    when ex $ removeDirectoryRecursive wdir
  where
    wdir = jobFolder

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
    wdir     = jobFolder
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
            runStep cabalExe [ "get"
                             , "--verbose=normal+nowrap+timestamp"
                             , "--index-state=@" <> T.pack (show jobIdxTs)
                             , pkgIdTxt
                             ]

    run StepSolve = do
        withCurrentDirectory wdir $ do
            T.writeFile "cabal.project" $ T.unlines
                [ "packages: " <> pkgIdTxt <> "/"
                , "with-compiler: " <> T.pack jobGhcExe
                , "tests: false"
                , "benchmarks: false"
                , "jobs: 1"
                , "build-log: logs/$libname.log"
                , "index-state: @" <> T.pack (show jobIdxTs)
                , "constraints: template-haskell installed" -- temporary hack for 7.8.4
                ]

            runStep cabalExe [ "new-build"
                             , "--verbose=normal+nowrap+timestamp"
                             , "--dry"]

    run StepFetchDeps = do
        msolve <- getStep StepSolve (Job{..})
        let mfetchPkgs = decodeTodoPlan . jsLog =<< msolve

        case mfetchPkgs of
          Nothing -> panic ("failed to decode job-log!\n\n" ++ show (fmap jsLog msolve))

          Just fetchPkgs -> do
              let pids = nub [ pid | (pid,_,_) <- fetchPkgs ]
              withCurrentDirectory wdir $
                  runStep cabalExe $ [ "fetch"
                                     , "--verbose=normal+nowrap+timestamp"
                                     , "--no-dependencies"
                                     ] ++ map (T.pack . display) pids

    run StepBuildDeps = RWL.withWrite jobBuildLock $ do
        withCurrentDirectory wdir $ do
            runStep cabalExe [ "new-build"
                             , "--verbose=normal+nowrap+timestamp"
                             , "--only-dependencies"
                             , pkgIdTxt
                             ]

    run StepBuild = RWL.withWrite jobBuildLock $ do
        withCurrentDirectory wdir $ do
            runStep cabalExe [ "new-build"
                             , "--verbose=normal+nowrap+timestamp"
                             , pkgIdTxt
                             ]

-- | Configures plan (if not done already), and waits for JobSolve
getJobSolve :: Job -> IO JobSolve
getJobSolve (Job{..}) = do
    jpFetch  <- getStep StepFetch (Job{..})
    jpSolve  <- getStep StepSolve (Job{..})

    -- TODO: make exception safe
    jpPlan <- case jpSolve of
      Nothing -> pure Nothing
      Just _  -> do
        planJsonRaw <- try $ LBS.readFile (wdir </> "dist-newstyle" </> "cache" </> "plan.json")
        case planJsonRaw of
          Left e -> (e::SomeException) `seq` pure Nothing -- fixme
          Right x -> evaluate $ J.decode x

    pure (JobSolve{..})
  where
    wdir = jobFolder

getJobBuildDeps :: Job -> IO JobBuildDeps
getJobBuildDeps (Job{..}) = do
    jrFetchDeps <- getStep StepFetchDeps (Job{..})
    jrBuildDeps <- getStep StepBuildDeps (Job{..})

    unitids0 <- try $ mapMaybe (stripExtension "log") <$> listDirectory (wdir </> "logs")
    let unitids = case unitids0 of
                    Left e -> (e :: SomeException) `seq` mempty
                    Right v -> v

    xs <- forM unitids $ \unitid -> do
        txt <- T.readFile (wdir </> "logs" </> unitid <.> "log")
        return (UnitID (T.pack unitid),txt)
    let jrBuildLogs = Map.fromList xs

    let alog = maybe [] (parseActionLog . jsLog) jrBuildDeps
        evconf = Set.fromList [ uid | (_,uid,EvConfig) <- alog ]
        evdone = Set.fromList [ uid | (_,uid,EvDone)   <- alog ]

        evdonet = Map.fromList [ (uid,pt) | (pt,uid,EvDone) <- alog ]
        jrBuildTimes = Map.fromList [ (uid,realToFrac $ pt1-pt0)
                                    | (pt0,uid,EvConfig) <- alog
                                    , Just pt1 <- [Map.lookup uid evdonet]
                                    ]

    jrFailedUnits <- case jrBuildDeps of
      Nothing -> pure mempty
      Just JobStep{..}
          | jsExitCode == 0 -> pure mempty
          | otherwise       -> pure (evconf Set.\\ evdone)

    pure (JobBuildDeps{..})
  where
    wdir = jobFolder

getJobBuild :: Job -> IO JobBuild
getJobBuild (Job{..}) = do
    jrBuild     <- getStep StepBuild     (Job{..})

    (jrBuildLogs2,jrFailedUnits2,jrBuildTimes2) <- case jrBuild of
               Nothing -> pure (mempty,mempty,mempty)
               Just JobStep{..} -> do
                   forM_ (T.lines jsLog) $ \l -> do
                       T.putStrLn ("| " <> l)
                   -- print (parseCompBlog jsLog)
                   let blogs :: [(UnitID,NonEmpty TsMsg)]
                       dts0  :: [(UnitID,NominalDiffTime)]
                       (blogs,dts0) = case parseCompBlog jsLog of
                         (prologue@(k:ks), [])
                             | Just cn <- findLegacyCN prologue
                               -> ([(cn, k:|ks)], [(cn, jsDuration)])
                         (_, units)
                               -> let tstarts, tdurs :: [NominalDiffTime]
                                      tstarts = map (fromMaybe (error "OHNO") . fst . NonEmpty.head . snd) units
                                      tlast = utcTimeToPOSIXSeconds jsStart + jsDuration
                                      tdurs = zipWith (-) (drop 1 tstarts ++ [tlast]) tstarts

                                  in (units, zip (map fst units) tdurs)

                   let fs = if jsExitCode == 0 then mempty
                            else (if null blogs then mempty else (Set.singleton $ fst $ last blogs))

                   return (Map.map (unlinesTS . toList) $ Map.fromList blogs, fs, Map.fromList dts0)

    pure (JobBuild{..})


cleanupTmp :: IO ()
cleanupTmp = handle hdlr $ do
    uid <- getEffectiveUserID
    fns <- listDirectory "/tmp"

    forM_ fns $ \fn -> do
        let fn' = "/tmp/" ++ fn
        st <- getSymbolicLinkStatus fn'
        when (fileOwner st == uid) $
            case fn of
              'c':'c':_:_:_:_:_ | isRegularFile st -> do
                              print fn'
                              removePathForcibly fn'

              'g':'h':'c':_:_ | isDirectory st -> do
                              print fn'
                              removePathForcibly fn'

              _ -> pure ()
  where
    hdlr :: SomeException -> IO ()
    hdlr e = print e
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

data EvType -- simple life-cycle
    = EvConfig
    | EvBuild
    | EvInstall
    | EvDone
    deriving (Eq,Ord,Show,Generic)

parseActionLog :: Text -> [(POSIXTime,UnitID,EvType)]
parseActionLog t0 = mapMaybe go (linesTS t0)
  where
    go :: TsMsg -> Maybe (POSIXTime,UnitID,EvType)
    go (Just pt, line :| []) = case T.words line of
                                 "Configuring":uid:_:_ -> Just (pt, UnitID uid, EvConfig)
                                 "Building"   :uid:_:_ -> Just (pt, UnitID uid, EvBuild)
                                 "Installing" :uid:_:_ -> Just (pt, UnitID uid, EvInstall)
                                 "Finished"   :uid:_:_ -> Just (pt, UnitID uid, EvDone)
                                 _ -> Nothing
    go _ = Nothing


parseFailedUnits :: Text -> [UnitID]
parseFailedUnits t0 = map go (drop 1 $ T.splitOn ("\nFailed to build ") t0)
  where
    go t = case T.words t of
      (_:"Build":"log":"(":logfn:"):":_) | Just t1' <- T.stripPrefix "logs/" logfn, Just t2' <- T.stripSuffix ".log" t1' -> UnitID t2'
      _ -> error ("unexpected msg structure")

findLegacyCN :: [TsMsg] -> Maybe UnitID
findLegacyCN = go
  where
    -- we're in a non-component build; grab the whole output
    -- w/o splitting for now (as otherwise we may miss
    -- `setup`-related failures); TODO: split off the action-plan maybe?
    go [] = Nothing
    go ((_,line:|_):rest)
      | "Configuring":pid:_ <- T.words line
      , Just uid <- T.stripSuffix "..." pid = Just $ UnitID (uid <> "-inplace")
      | otherwise = go rest

-- parseCompBlog :: T.Text -> Maybe [(UnitID,T.Text)]
parseCompBlog :: Text -> ([TsMsg],[(UnitID, NonEmpty TsMsg)])
parseCompBlog t0 = case go Nothing [] . linesTS $ t0 of
                     (Nothing,ls):rest -> (toList ls, fromMaybe (error "parseCompBlog") $ mapM j rest)
                     rest              -> ([],        fromMaybe (error "parseCompBlog") $ mapM j rest)
  where
    go :: Maybe UnitID -> [TsMsg] -> [TsMsg] -> [(Maybe UnitID, NonEmpty TsMsg)]
    go cn0 ls0 []
      | k:ks <- reverse ls0 = [(cn0,k:|ks)]
      | otherwise           = []
    go cn0 ls0 (tsmsg1@(_,(line1:|_)):rest)
      | ("Configuring":"component":cname:"from":pid:_) <- T.words line1
        = case (parseCompName cname) of
            Nothing -> error "parseCompName: unvalid compname"
            Just cn
              | k:ks <- reverse ls0
                -> (cn0,k:|ks) : go (Just $ mkCN pid cn) [tsmsg1] rest
              | otherwise     -> go (Just $ mkCN pid cn) [tsmsg1] rest

      | otherwise = go cn0 (tsmsg1 : ls0) rest

    mkCN pid0 cn = UnitID (pid <> "-inplace" <> maybe "" ("-"<>) (strCompName cn))
      where
        pid = maybe pid0 id $ T.stripSuffix "..." pid0

    j (Just k,v) = Just (k,v)
    j (Nothing,_) = Nothing

-- parseBLog :: T.Text -> [(UnitID,Text)]
-- parseBLog txt = map go (drop 1 $ T.splitOn ("\nConfiguring ") txt)
--   where
--     go t = case T.words t of
--              -- we're in a non-component build; grab the whole output
--              -- w/o splitting for now (as otherwise we may miss
--              -- `setup`-related failures); TODO: split off the action-plan maybe?
--              (pid:_) | Just uid <- T.stripSuffix "..." pid -> (UnitID (uid <> "-inplace"), txt)
--              ("component":cname:"from":pid:_)
--                    | Just cn <- parseCompName cname -> (mkCN pid cn, "Configuring " <> t)
--              _ -> error (show t)

--     mkCN pid cn = UnitID (pid <> "-inplace" <> maybe "" ("-"<>) (strCompName cn))

-- decodes action-plan
decodeTodoPlan :: T.Text -> Maybe [(PkgId,UnitID,Bool)] -- True if needs download
decodeTodoPlan s0
--  | traceShow (map (map decodeTodoLine) todos) False = undefined
  | ["Up to date" :| []] == take 1 s0' = Just mempty
  | [todolst] <- todos = mapM decodeTodoLine todolst
  | otherwise = Nothing
  -- | otherwise = do
  --   ("Resolving dependencies...":"In order, the following would be built (use -v for more details):":ls) <- pure s0'
  --   pkgs <- mapM (decLine . T.unpack) ls
  --   pure (Set.fromList pkgs)
  where
    s0' = drop 1 $ dropWhile (/= (marker1:|[])) $ map snd $ linesTS s0

    marker1 = "Resolving dependencies..."
    marker2 = "In order, the following would be built (use -v for more details):"

    todos = map NonEmpty.tail $ filter ((marker2 ==) . NonEmpty.head) s0'


decodeTodoLine :: Text -> Maybe (PkgId,UnitID,Bool)
decodeTodoLine t0 = do
    "-":pid0:unitid0:rest0 <- pure (T.words t0)
    let rest = T.unwords rest0
        isDown = T.isInfixOf "requires download" rest
    unitid'  <- T.stripPrefix "{" unitid0
    unitid'' <- T.stripSuffix "}" unitid'
    let unitid = UnitID unitid''
    pid      <- simpleParse (T.unpack pid0)
    pure (pid,unitid,isDown)

panic :: String -> IO a
panic msg = do
    putStrLn "=== PANIC ====================================================================="
    putStrLn msg
    putStrLn "==============================================================================="
    fail msg

-- TODO/FIXME: assumes length of timestamp == 14; this will become an
-- error once we reach 11-digit posix-seconds
linesTS :: Text -> [TsMsg]
linesTS = go0 . T.lines
  where
    go0 = go Nothing []

    go :: Maybe POSIXTime -> [Text] -> [Text] -> [TsMsg]
    go pt      ls [] = case ls of
                         [] -> []
                         (x:xs) -> [(pt,x:|xs)]
    go Nothing [] (x:xs)
      | Just (pt,l) <- splitTS x = go (Just pt) [l] xs
      | otherwise = (Nothing,x:|[]) : go0 xs
    go mpt@(Just _) ls@(k:ks) (x:xs)
      | Just (pt,l) <- splitTS x = (mpt,k:|ks) : go (Just pt) [l] xs
      | Just l  <- splitCont x   = go mpt (ls++[l]) xs
      | otherwise = (mpt,k:|ks) : go0 (x:xs)
    go _ _ (_:_) = error "linesTS: the impossible has happened"

    splitCont :: T.Text -> Maybe T.Text
    splitCont = T.stripPrefix "               " -- 14+1 whitespace

-- FIXME/TODO, make better inverse of linesTS
unlinesTS :: [TsMsg] -> Text
unlinesTS = T.unlines . concatMap (toList . snd)

splitTS :: T.Text -> Maybe (POSIXTime,T.Text)
splitTS t = do
    let (tsstr,rest) = T.break (==' ') t
    guard (T.isPrefixOf " " rest)
    [t1,t2] <- pure (T.splitOn "." tsstr)
    guard (T.all isDigit t1)
    guard (T.all isDigit t2)
    guard (T.length t1 == 10) -- fixme
    guard (T.length t2 == 3)

    -- weird.. for some reason we can't 'read "123.456" :: Rational'
    let ts = ((read $ T.unpack t1)*1000 + (read $ T.unpack t2)) % 1000

    pure (fromRational ts,T.drop 1 rest)

