{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ViewPatterns      #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Prelude.Local

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.Aeson as J
import Data.Char
import qualified Data.Aeson.Types as J
import           Data.List
import           Text.Groom
-- import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import qualified Data.UUID.Types as UUID
import           Data.UUID.Types (UUID)

import           Data.Profunctor.Product -- (p2, p3)
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Types as PGS
import           Database.PostgreSQL.Simple.Types (Only(..))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Data.Pool

import           IndexHelper
import           Job
import           PkgId
import           System.Exit
import           WorkerApi
import           WorkerApi.Client
import           Control.Monad.Except (ExceptT, runExceptT)
-- import Data.Aeson
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutNone)
import           Servant.API
import           Servant.Client

import qualified Data.HashSet as HS
import qualified Snap.Util.FileServe as Snap
import           System.Environment

import           PlanJson as PJ

import qualified Control.Concurrent.FairRWLock as RW

ptime2utc :: PkgIdxTs -> UTCTime
ptime2utc = posixSecondsToUTCTime . fromIntegral

cabalExe :: FilePath
cabalExe = "cabal"

-- TODO: store in database or config file; allow multiple workers
workerUri :: BaseUrl
workerUri = BaseUrl Http "matrix-3-worker-1" 8001 "/api"

data CtrlConf = CtrlConf
    { ccPort    :: Word16
    , ccWorkers :: [(BaseUrl,CompilerID)]
    } deriving (Show)

readConfig :: FilePath -> IO CtrlConf
readConfig _fn = do
    -- hack/fixme
    let ccPort = 8080
        ccWorkers =
            [ (BaseUrl Http "matrix-3-worker-1" 8001 "/api", ghc_7_8)
            , (BaseUrl Http "matrix-3-worker-2" 8001 "/api", ghc_7_10)
            , (BaseUrl Http "matrix-3-worker-3" 8001 "/api", ghc_8_0)
            , (BaseUrl Http "matrix-3-worker-4" 8001 "/api", ghc_7_6)
            , (BaseUrl Http "matrix-3-worker-5" 8001 "/api", ghc_7_4)
            ]

        Just [ghc_8_0,ghc_7_10,ghc_7_8,ghc_7_6,ghc_7_4] = mapM simpleParse ["ghc-8.0.2","ghc-7.10.3","ghc-7.8.4","ghc-7.6.3","ghc-7.4.2"]

    pure $ (CtrlConf {..})

-- for PG's benefit
newtype UUIDs = UUIDs { unUUIDs :: [UUID] }
    deriving (Eq,Generic,Show)

instance ToField UUIDs where
    toField (UUIDs xs) = Many [toField (PGS.PGArray xs), Plain "::uuid[]" ]

-- TODO
type HcID = PkgId

data App = App
  { appDbPool :: Pool PGS.Connection
  , appQThreads :: RW.RWLock
  , appWorkers :: [(BaseUrl,CompilerID)]
  }

timeOp :: String -> IO a -> IO a
timeOp l act = do
    t0 <- getPOSIXTime
    res <- act
    t1 <- getPOSIXTime
    print (l, t1-t0)
    return res

pkgIdxTupleToDB :: PkgIdxTuple -> (Text,Text,Int,Int,Text)
pkgIdxTupleToDB PkgIdxTuple{..} = (pn, ver, rev, t, pitOwner)
  where
    pn  = T.pack $ display pitName
    ver = maybe "" (T.pack . display) $ pitVer
    rev = fromIntegral pitRev
    t   = fromIntegral pitTime

data PkgIdxKey = PkgIdxKey !Text !Text !Int
               deriving (Generic,Eq,Ord)

instance Hashable PkgIdxKey
instance NFData   PkgIdxKey
instance PGS.FromRow PkgIdxKey

-- untested
jobid2xunitids :: PGS.Connection -> JobId -> IO (Set UUID)
jobid2xunitids dbc jid =
    (Set.fromList . map fromOnly) <$>
    PGS.query dbc
        "WITH RECURSIVE t(xunitid) AS \
            \(SELECT unnest(units) \
               \FROM iplan_job \
               \WHERE jobid = ? \
            \UNION \
            \SELECT unnest(c.lib_deps || c.exe_deps) \
               \FROM iplan_comp c, t \
               \WHERE c.xunitid = t.xunitid ) \
            \SELECT xunitid FROM t" (Only jid) -- JOIN iplan_unit USING (xunitid)


registerPkgIds :: PGS.Connection -> Set PkgId -> IO ()
registerPkgIds dbconn pids = do
    res1 <- PGS.executeMany dbconn "INSERT INTO pkgname (pname) VALUES (?) ON CONFLICT DO NOTHING" [ Only n | n <- Set.toList pnames ]
    putStrLn ("new package names: " ++ show res1)

    res1a <- PGS.executeMany dbconn "INSERT INTO version (pver) VALUES (?) ON CONFLICT DO NOTHING" [ Only v | v <- Set.toList pvers ]
    putStrLn ("new versions: " ++ show res1a)

    res1c <- PGS.executeMany dbconn "INSERT INTO pkgver (pname,pver) VALUES (?,?) ON CONFLICT DO NOTHING" [ (n,v) | PkgId n v <- Set.toList pids ]
    putStrLn ("new pkgver entries: " ++ show res1c)
  where
    pnames = Set.fromList  [ n | PkgId n _ <- Set.toList pids ]
    pvers  = Set.fromList  [ v | PkgId _ v <- Set.toList pids ]

initWorkers :: PGS.Connection -> [(BaseUrl,CompilerID)] -> IO ()
initWorkers dbconn wrks = do
    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })

    forM_ wrks $ \(wuri,gv) -> do

        res <- runExceptT $ listPkgDbGlobal gv manager wuri
        -- (either (fail . show) pure =<< runExceptT) $ do

        -- gvs' <- map fromOnly <$> liftIO (PGS.query_ dbconn "SELECT compiler FROM hscompiler")

            -- WorkerInfo{..} <- getWorkerInfo manager workerUri
            -- listPkgDbGlobal gv manager

        case res of
          Left e -> fail (show e)
          Right gpkgs -> do
              -- TODO: register also as 'builtin' units
              let pids = Set.fromList (map gpiPkgId gpkgs)
              mapM_ print pids
              registerPkgIds dbconn pids


performIndexUpdate :: PGS.Connection -> IO ()
performIndexUpdate dbconn = do
    t0 <- getPOSIXTime
    putStrLn "reading db index"
    oldents <- (evaluate . force . HS.fromList) =<< PGS.query_ dbconn "SELECT pname,pver,prev FROM pkgindex"
    t1 <- getPOSIXTime
    putStrLn $ "reading db index took " ++ show (t1-t0)

    putStrLn "reading index..."
    idxtups <- readIndexTuples indexTar
    pindex' <- evaluate . force . filter (\(a,b,c,_,_) -> not (HS.member (PkgIdxKey a b c) oldents) && b /= "") . map pkgIdxTupleToDB $ idxtups
    let newPkgIds = Set.fromList [ PkgId pn pv | (pn0,pv0,_,_,_) <- pindex'
                                               , Just pn <- [simpleParse (T.unpack pn0)]
                                               , Just pv <- [simpleParse (T.unpack pv0)]
                                               ]

    t2 <- getPOSIXTime
    putStrLn $ "reading index took " ++ show (t2-t1)

    putStrLn "registering new db entries..."

    registerPkgIds dbconn newPkgIds

    res1b <- PGS.executeMany dbconn "INSERT INTO idxstate (ptime) VALUES (?) ON CONFLICT DO NOTHING" [Only t | (_,_,_,t,_) <- pindex']
    putStrLn ("new idxstate entries: " ++ show res1b)

    res2 <- PGS.executeMany dbconn "INSERT INTO pkgindex (pname,pver,prev,ptime,powner) VALUES (?,?,?,?,?) ON CONFLICT DO NOTHING" pindex'
    putStrLn ("new index entries: " ++ show res2)

    t3 <- getPOSIXTime
    putStrLn $ "updating db took " ++ show (t3-t2)

    pure ()


{- hacky code for importing tags.json

    tmp <- BS.readFile "tags.json"
    let Just foo = J.decodeStrict tmp :: Maybe (ListSlice TagListEntry)
    let foo2 = nub [ (teName ent, pn) | ent <- lsItems foo, pn <- tePackages ent ]
    withResource appDbPool $ \dbconn -> do
        res <- PGS.executeMany dbconn "INSERT INTO pname_tag (tagname,pname) values (?,?)" foo2
        print res
-}



planJson2Dp :: J.Value -> IO ()
planJson2Dp planJsonRaw = do
    let J.Success pj@(PlanJson{..}) = J.fromJSON planJsonRaw

    putStrLn (groom $ (pj :: PlanJson))

    putStrLn ""
    putStrLn "----------------------------------------------------------------------------"
    putStrLn "----------------------------------------------------------------------------"
    putStrLn ""

    mapM_ (putStrLn . groom) $ planJson2DbUnitComps mempty pj

planItemAllDeps :: PlanItem -> Set UnitID
planItemAllDeps PlanItem{..} = mconcat [ ciLibDeps <> ciExeDeps | CompInfo{..} <- Map.elems piComps ]

planJsonIdGrap :: PlanJson -> Map UnitID (Set UnitID)
planJsonIdGrap PlanJson{..} = Map.map planItemAllDeps pjItems

-- we assume that 'show' doesn't use non-latin1 code-points
toUUID :: Show a => a -> UUID
toUUID = uuidHash . BS.pack . show

-- NB: emits DB rows in topological order, i.e. not violating FK-constraints
planJson2DbUnitComps :: Map UnitID (IPStatus,Text,Maybe NominalDiffTime) -> PlanJson -> [(DB_iplan_unit,[DB_iplan_comp])]
planJson2DbUnitComps smap PlanJson{..} = go mempty topoUnits
    -- let rootunits = [ piId | PlanItem{..} <- Map.elems pjItems, piType == PILocal ]
    -- print rootunits
    -- let topoUnits = toposort (planJsonIdGrap PlanJson{..})
    -- forM_ topoUnits print
    -- mapM_ (putStrLn . groom) $ go mempty topoUnits
  where
    topoUnits = toposort (planJsonIdGrap PlanJson{..})

    go :: (Map UnitID UUID) -> [UnitID] -> [(DB_iplan_unit,[DB_iplan_comp])]
    go _ [] = []
    go m (uid0:uids) = (DB_iplan_unit xuid piId pjCompilerId piType pn pv jflags pkind logmsg dt, cs)
                       : go (Map.insert uid0 xuid m) uids
      where
        xuid = case piType of
                 -- short-cut, we trust the unit-id for global packages to be unique within comp/os/arch
                 PIGlobal -> toUUID (unUnitID uid0,display pjCompilerId,pjOs,pjArch)
                 -- in all other cases, the unit-id is not unique outside plan.json
                 _ -> toUUID (unUnitID uid0,display piPId,display pjCompilerId,pjOs,pjArch,piType,Map.toAscList piFlags,cs')

        pkind = stat ^? _Just._1

        logmsg = case stat ^? _Just._2 of
                   Nothing -> Nothing
                   Just "" -> Nothing
                   Just t  -> Just t

        dt = stat ^? _Just._3._Just

        stat = case piType of
                 PIBuiltin -> Just (IPOk,"",Nothing)
                 _         -> Map.lookup uid0 smap

        Just PlanItem{..} = Map.lookup uid0 pjItems
        PkgId pn pv = piPId

        jflags = toJSON piFlags

        cs' = [ (cn,(sort $ map lupUUID $ Set.toList ciLibDeps),(sort $ map lupUUID $ Set.toList ciExeDeps))
              | (cn,CompInfo{..}) <- Map.toAscList piComps ]

        cs = [ DB_iplan_comp xuid cn (UUIDs ldeps) (UUIDs edeps) | (cn,ldeps,edeps) <- cs' ]

        lupUUID k = Map.findWithDefault (error "lupUUID") k m

-- | Build-status for a build-unit
data IPStatus = IPOk
              | IPBuildFail
              | IPBuildDepsFail
              deriving (Eq,Ord,Show,Generic)

instance NFData IPStatus

instance ToField IPStatus where
    toField = toField . go
      where
        go :: IPStatus -> Text
        go = \case
            IPOk            -> "ok"
            IPBuildFail     -> "fail"
            IPBuildDepsFail -> "fail_deps"

instance FromField IPStatus where
    fromField _f mdata = return (go mdata) -- FIXME, check type
      where
        go (Just "ok")        = IPOk
        go (Just "fail")      = IPBuildFail
        go (Just "fail_deps") = IPBuildDepsFail
        go _ = error ("FromField(IPStatus) " ++ show mdata)

data DB_iplan_unit = DB_iplan_unit UUID UnitID HcID PIType PkgN Ver J.Value (Maybe IPStatus) (Maybe Text) (Maybe NominalDiffTime)
                   deriving (Show,Generic)

db_iplan_unit_insert :: PGS.Query
db_iplan_unit_insert = "INSERT INTO iplan_unit(xunitid,unitid,compiler,pkind,pname,pver,flags,bstatus,logmsg,dt) VALUES (?,?,?,?,?,?,?,?,?,?)"

instance PGS.ToRow DB_iplan_unit

----

data DB_iplan_comp = DB_iplan_comp UUID CompName UUIDs UUIDs -- Vector?
                   deriving (Show,Generic)

-- FIXME: doesn't work with execute-many
db_iplan_comp_insert :: PGS.Query
db_iplan_comp_insert = "INSERT INTO iplan_comp(xunitid,cname,lib_deps,exe_deps) VALUES (?,?,?,?)"

instance PGS.ToRow DB_iplan_comp

----

data DB_iplan_job = DB_iplan_job UUID PkgN Ver HcID J.Value UUIDs
                  deriving (Generic,Show)

db_iplan_job_insert :: PGS.Query
db_iplan_job_insert = "INSERT INTO iplan_job(jobid,pname,pver,compiler,plan,units) VALUES (?,?,?,?,?,?)"

instance PGS.ToRow DB_iplan_job


doNothing :: PGS.Query -> PGS.Query
doNothing = flip mappend " ON CONFLICT DO NOTHING"

queryJobExists :: PGS.Connection -> UUID -> IO Bool
queryJobExists dbconn jid = do
    PGS.query dbconn "SELECT EXISTS (SELECT 1 FROM iplan_job WHERE jobid = ?)" (Only jid) >>= \case
        [Only exists] -> pure exists
        _ -> fail "queryJobExists: the impossible happened"



-- pname,prio,modified,ptime
-- data DB_queue = DB_queue PkgN Int UTCTime PkgIdxTs
--               deriving (Show,Generic)
-- instance PGS.FromRow DB_queue

queryNextJobTask :: PGS.Connection -> [CompilerID] -> PkgN -> PkgIdxTs -> IO (Maybe (Ver,CompilerID))
queryNextJobTask _ [] _ _ = pure Nothing
queryNextJobTask dbconn cids pname ptime = do
    pvgvs <- PGS.query dbconn
             "SELECT pver,compiler FROM pkgindex, hscompiler WHERE pname = ? AND ptime <= ? AND compiler IN ? \
             \EXCEPT \
             \SELECT pver,compiler FROM pkg_blacklist, hscompiler WHERE pname = ? \
             \EXCEPT \
             \SELECT pver,compiler FROM iplan_job JOIN solution USING (jobid) WHERE pname = ? AND ptime = ? \
             \EXCEPT \
             \SELECT pver,compiler FROM solution_fail WHERE pname = ? AND ptime = ?"
             ( pname,ptime,PGS.In cids
             , pname
             , pname,ptime
             , pname,ptime
             )

    pvpts <- Map.fromList <$>
             PGS.query dbconn "SELECT pver,max(ptime) FROM pkgindex WHERE pname = ? AND ptime <= ? GROUP BY pver" (pname,ptime)

    let pverOrd :: Ver -> Maybe PkgIdxTs
        pverOrd v = Map.lookup v pvpts

    -- TODO: order by max(ptime) of entries

    case pvgvs of
      [] -> pure Nothing
      _  -> pure (Just $ maximumBy (comparing (first pverOrd)) pvgvs)


queryQEntries :: PGS.Connection -> IO [QEntry]
queryQEntries dbconn =
    PGS.query_ dbconn "SELECT prio,modified,pname,ptime FROM queue ORDER BY prio desc, modified desc, ptime desc, pname asc"

-- background thread
scheduler :: App -> IO ()
scheduler App{..} = do
    thrIds <- forM appWorkers $ \(wuri,cid) -> forkIO (forever $ go wuri cid)

    let allCids = map snd appWorkers

    forever $ do
        putStrLn ("MARK")

        withResource appDbPool $ \dbconn -> do
            qents <- queryQEntries dbconn
            forM_ qents $ \QEntry{..} -> do
                let Just ptime = qIdxState
                queryNextJobTask dbconn allCids qPackageName ptime >>= \case
                    Nothing -> do
                        putStrLn "all done for q-entry; deleting..."
                        _ <- PGS.execute dbconn "DELETE FROM queue WHERE pname = ?" (Only qPackageName)
                        pure ()

                    Just _ -> pure ()

        threadDelay 3000000

  where
    go wuri cid = RW.withRead appQThreads $ do
        mtask <- withResource appDbPool $ \dbconn -> do
            qents <- queryQEntries dbconn

            let queryNextJobTask' :: QEntry -> IO (Maybe (PkgId,CompilerID,PkgIdxTs))
                queryNextJobTask' q0 = do
                    let Just ptime = qIdxState q0
                    mres <- queryNextJobTask dbconn [cid] (qPackageName q0) ptime
                    pure $ case mres of
                      Nothing        -> Nothing
                      Just (pv,cid') -> Just (PkgId (qPackageName q0) pv, cid', ptime)
            firstJustM queryNextJobTask' qents

        case mtask of
          Nothing -> do
              putStrLn ("nothing to do left in queue; sleeping a bit... " ++ show cid)
              threadDelay 3000000

          Just (pkgid,gv,idxts) -> do
              putStrLn "============================================================================"
              print (gv,pkgid,idxts)
              putStrLn "============================================================================"
              go2 wuri gv pkgid idxts

    initWJob gv pid idxts wuri = do
        manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
        CreateJobRes wjid <- either (fail . show) pure =<< runExceptT
                             (createJob (CreateJobReq gv (Just idxts) pid) manager wuri)
        pure (wjid,manager,wuri)

    doneWJob gv (wjid,manager,wuri) = do
        NoContent <- either (fail . show) pure =<< runExceptT (destroyJob wjid manager wuri)

        pis <- either (fail . show) pure =<< runExceptT (listPkgDbStore gv manager wuri)
        let pisLen = length pis
        putStrLn ("store size = " ++ show pisLen)

        when (pisLen > 1000) $ do
            NoContent <- either (fail . show) pure =<< runExceptT (destroyPkgDbStore gv manager wuri)
            pure ()

    go2 :: BaseUrl -> CompilerID -> PkgId -> Int -> IO ()
    go2 wuri0 ghcver pid idxts = bracket (initWJob ghcver pid idxts wuri0) (doneWJob ghcver) $ \(wjid,manager,wuri) -> do

        sinfo <- either (fail . show) pure =<< runExceptT
                 (getJobSolveInfo wjid manager wuri)

        let PkgId pidn pidv = pid

        putStrLn "----------------------------------------------------------------------------"

        case jpPlan sinfo of
          Nothing -> do
              case jpSolve sinfo of
                Nothing -> do
                    putStrLn "fetch-step failed? WTF?!?"
                    withResource appDbPool $ \dbconn -> do
                        _ <- PGS.execute dbconn (doNothing "INSERT INTO pkg_blacklist(pname,pver) VALUES (?,?)") (pidn,pidv)
                        pure ()
                Just solvejs -> do
                    withResource appDbPool $ \dbconn -> do
                        foo1 <- PGS.execute dbconn (doNothing "INSERT INTO solution_fail(ptime,pname,pver,compiler,solvererr,dt) VALUES (?,?,?,?,?,?)")
                                (idxts,pidn,pidv,ghcver,jsLog solvejs,jsDuration solvejs)
                        print foo1 --print (jsLog solvejs)
          Just jplan -> do
              pj <- case J.fromJSON jplan of
                      J.Error e -> fail e
                      J.Success x -> pure x

              -- compute job-id from plan.json only
              let dbunits0 = planJson2DbUnitComps mempty pj
                  dbJobUids = sort [ xuid | (DB_iplan_unit xuid _ _ PILocal pn pv _ _ _ _,_) <- dbunits0
                                          , pn == pidn, pv == pidv ]
                  dbJobId = toUUID (display pid, display (pjCompilerId pj), pjOs pj, pjArch pj, dbJobUids) -- maybe hash over jplan?

              -- check whether job-id already exists in DB
              -- TODO: check whether we need to (re)compute results
              jobExists <- withResource appDbPool $ \dbconn -> queryJobExists dbconn dbJobId

              putStrLn $ "Job " ++ show dbJobId ++ (if jobExists then " exists already!" else " doesn't exist already")

              unless jobExists $ do

                  bdinfo <- either (fail . show) pure =<< runExceptT
                            (getJobBuildDepsInfo wjid manager wuri)

                  binfo <- either (fail . show) pure =<< runExceptT
                           (getJobBuildInfo wjid manager wuri)

                  let stats0 =
                          Map.fromList [ (k,(if k `Set.member` (jrFailedUnits bdinfo <> jrFailedUnits2 binfo) then IPBuildFail else IPOk,v))
                                       | (k,v) <- Map.toList $ (jrBuildLogs bdinfo <> jrBuildLogs2 binfo)
                                       ]
                          `mappend` -- NB: first entry is retained
                          Map.fromList [ (k,(IPBuildFail,"")) | k <- (Set.toList $ jrFailedUnits bdinfo <> jrFailedUnits2 binfo) ]

                      stats = Map.fromList [ (k,(st,lm,Map.lookup k (jrBuildTimes bdinfo <> jrBuildTimes2 binfo)))
                                           | (k,(st,lm)) <- Map.toList stats0 ]

                  -- compute real dbunits
                  let dbunits = planJson2DbUnitComps stats pj

                  withResource appDbPool $ \dbconn -> do
                      let rows1 = map fst dbunits
                      foo1 <- PGS.executeMany dbconn (db_iplan_unit_insert `mappend`
                                                      " ON CONFLICT (xunitid) \
                                                      \ DO UPDATE SET bstatus = EXCLUDED.bstatus, logmsg = EXCLUDED.logmsg, dt = EXCLUDED.dt \
                                                      \ WHERE iplan_unit.bstatus IS NULL") $
                              rows1
                      print (foo1,length rows1)

                      -- TODO: update entries with NULL status

                      let rows2 = concatMap snd dbunits
                      foo2 <- PGS.executeMany dbconn (doNothing db_iplan_comp_insert) $
                              concatMap snd dbunits
                      print (foo2,length rows2)

                      foo3 <- PGS.execute dbconn (doNothing db_iplan_job_insert) $
                              DB_iplan_job dbJobId pidn pidv (pjCompilerId pj) jplan (UUIDs dbJobUids)
                      print foo3

                      -- forward-propagate fail_deps
                      whileM_ $ do
                          foo5 <- PGS.execute_ dbconn "UPDATE iplan_unit SET bstatus = 'fail_deps' \
                                                      \WHERE xunitid IN (SELECT DISTINCT a.xunitid FROM iplan_unit a \
                                                                        \JOIN unit_dep ON (a.xunitid = parent) \
                                                                        \JOIN iplan_unit b ON (b.xunitid = child) \
                                                                        \WHERE a.bstatus IS NULL AND b.bstatus IN ('fail','fail_deps'))"
                          putStrLn $ "=> " ++ show foo5
                          pure (foo5 /= 0)

              -- in any case, register a solution now
              withResource appDbPool $ \dbconn -> do
                  putStrLn "registering solution"
                  foo4 <- PGS.execute dbconn (doNothing "INSERT INTO solution(ptime,jobid,dt) VALUES (?,?,?)")
                          (idxts,dbJobId,jsDuration <$> jpSolve sinfo)
                  print foo4


main :: IO ()
main = do
    args <- getArgs

    cconf <- readConfig undefined

    -- simple hack; hijack CLI and perform db index update job if called with special argument
    -- TODO: proper CLI parser with subcommands
    runQ <- case args of
      "update":_ -> do
          bracket mkConn killConn performIndexUpdate
          exitSuccess
      "init":_ -> do
          bracket mkConn killConn (\c -> initWorkers c (ccWorkers cconf))

          exitSuccess

      "plan":fns -> do
          -- bracket mkConn killConn performIndexUpdate
          forM_ fns $ \fn -> do
            putStrLn $ "processing " ++ show fn
            Just pj <- J.decodeStrict <$> BS.readFile fn
            planJson2Dp pj
            putStrLn "============================================================================"
          putStrLn "DONE"
          exitSuccess

      [] -> pure False
      ["runqueue"] -> pure True

      _ -> fail "bad CLI arguments"

    print cconf

    putStrLn "start"

    appDbPool <- createPool mkConn killConn 1 10.5 4
    -- _appBootTime <- getPOSIXTime
    -- _appJobs <- newTVarIO mempty

    appQThreads <- RW.new
    let appWorkers = ccWorkers cconf

    let app = App{..}

    if runQ then do
        putStrLn "starting queue runner"
        _threadid <- forkIO (scheduler app)
        pure ()
    else do
        putStrLn "*NOT* starting queue runner"
        RW.acquireWrite appQThreads

    runController app (if runQ then 8080 else 8081)

    when runQ $ do
        putStrLn "waiting for scheduler to halt..."
        RW.acquireWrite appQThreads
        putStrLn "...halted!"

    pure ()
  where
    mkConn = do
        putStrLn "opening new dbconn"
        PGS.connect (PGS.ConnectInfo "" 0 "" "" "matrix")

    killConn c = do
        putStrLn "closing a dbconn"
        PGS.close c


runController :: App -> Int -> IO ()
runController !app port =
    serveSnaplet (setPort port defaultConfig) initApp
  where
    initApp :: SnapletInit App App
    initApp = makeSnaplet "matrix-controller" "Matrix CI controller" Nothing $ do
        addRoutes [("/api/", apiHandler)
                  ,("/package/", Snap.serveFile "ui/index.html")
                  ,("/packages/", Snap.serveFile "ui/index.html")
                  ,("/latest/", Snap.serveFile "ui/index.html")
                  ,("/", uiHandler)
                  ]
        return app

    apiHandler :: AppHandler ()
    apiHandler = do
        serveSnap controllerApi server

    uiHandler :: AppHandler ()
    uiHandler = Snap.serveDirectory "ui"


controllerApi :: Proxy (ControllerApi AppHandler)
controllerApi = Proxy

type TagName = T.Text

data TagListEntry = TagListEntry
    { teName     :: !TagName
    , tePackages :: [PkgN]
    } deriving (Generic)

data PkgListEntry = PkgListEntry
    { pleName   :: !PkgN
    , pleTags   :: [TagName]
    , pleReport :: Maybe UTCTime
    } deriving (Generic)

data PkgListEntry2 = PkgListEntry2
    { ple2PackageName :: !PkgN
    , ple2Modified    :: !UTCTime
    } deriving (Generic,Eq,Ord,Show)

data ListSlice a = ListSlice
    { lsOffset :: !Word
    , lsCount  :: !Word
    , lsItems  :: [a]
    } deriving (Generic)

data PkgVerInfo = PkgVerInfo
    { pviName :: PkgN
    , pviVersions :: [PkgVerInfoEntry]
    } deriving (Generic)

data PkgVerInfoEntry = PkgVerInfoEntry
    { pvieVersion    :: !Ver
    , pvieRevision   :: !Word
    , pviePreference :: !T.Text
    } deriving (Generic,Eq,Ord)

data QPrio = QPlow
           | QPmedium
           | QPhigh
           deriving (Generic,Eq,Ord,Enum,Bounded,Show)

instance FromField QPrio where
    fromField f mdata = fromInt <$> fromField f mdata
      where
        fromInt :: Int -> QPrio
        fromInt i
          | i >=  10   = QPhigh
          -- medium
          | i <= -10   = QPlow

          | otherwise  = QPmedium


instance ToField QPrio where
    toField = toField . toInt
      where
        toInt :: QPrio -> Int
        toInt = \case
            QPhigh   ->  10
            QPmedium ->   0
            QPlow    -> -10

data QEntry = QEntry
    { qPriority    :: QPrio
    , qModified    :: Maybe UTCTime
    , qPackageName :: PkgN
    , qIdxState    :: Maybe PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow QEntry

instance ToJSON a => ToJSON (ListSlice a) where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON a => FromJSON (ListSlice a) where { parseJSON = myParseJSON }

instance ToJSON   TagListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON TagListEntry where { parseJSON = myParseJSON }

instance ToJSON   QPrio where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON QPrio where { parseJSON = myParseJSON }

instance ToJSON   QEntry where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntry where { parseJSON = myParseJSONCml }

instance ToJSON PkgListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToJSON PkgListEntry2 where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }

instance ToJSON PkgVerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToJSON PkgVerInfoEntry where { toJSON = myToJSON; toEncoding = myToEncoding }

mkListSlice :: Word -> [a] -> ListSlice a
mkListSlice ofs xs = ListSlice ofs (fromIntegral $ length xs) xs

type ControllerApi m =
       "v1.0.0" :> "tag"                 :> "list" :> ListOp TagListEntry
  :<|> "v1.0.0" :> "tag"                 :> "name" :> Capture "tagname" TagName :> ReqBody '[JSON] PkgN :> Put '[JSON] ()
  :<|> "v1.0.0" :> "tag"                 :> "name" :> Capture "tagname" TagName :> ReqBody '[JSON] PkgN :> Delete '[JSON] ()

  :<|> "v1.0.0" :> "package"             :> "list" :> ListOp PkgListEntry
  :<|> "v1.0.0" :> "package"             :> "list-latest-reports" :> ListOp PkgListEntry2
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> Get '[JSON] PkgVerInfo
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "tags" :> Post '[JSON] [TagName]
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "latest" :> Get '[JSON] JobReport
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "latest" :> "cell" :> "id" :> Capture "cellid" Text :> Get '[JSON] CellReport

  :<|> "v1.0.0" :> "queue"               :> "list"                                                     :> ListOp QEntry
  :<|> "v1.0.0" :> "queue"                                                   :> ReqBody '[JSON] QEntry :> Post '[JSON] ()
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN                           :> Get '[JSON] QEntry
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN :> ReqBody '[JSON] QPrio  :> Put '[JSON] QEntry
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN                           :> Delete '[JSON] ()

type ListOp e = QueryParam "count" Word :> Post '[JSON] (ListSlice e)

server :: Server (ControllerApi AppHandler) AppHandler
server = tagListH
    :<|> tagSetH
    :<|> tagDelH

    :<|> pkgListH
    :<|> llrListH
    :<|> pkgVerInfoH
    :<|> pkgTagsH
    :<|> pkgLastReport
    :<|> pkgCellReport

    :<|> queListH
    :<|> quePostH
    :<|> queGetH
    :<|> quePutH
    :<|> queDelH
  where
    ----------------------------------------------------------------------------
    -- tag ---------------------------------------------------------------------

    tagListH _cnt = do
        withDbc $ \dbconn -> do
            tags <- queryAllTags dbconn
            pure $ mkListSlice 0 [ TagListEntry tn pns | (tn,pns) <- Map.toList tags ]

    tagSetH, tagDelH :: TagName -> PkgN -> AppHandler ()
    tagSetH tagn pkgn = withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "INSERT INTO pname_tag (tagname,pname) values (?,?) ON CONFLICT DO NOTHING" (tagn,pkgn)
            pure ()

    tagDelH tagn pkgn = withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "DELETE FROM pname_tag WHERE tagname = ? AND pname = ?" (tagn,pkgn)
            pure ()

    ----------------------------------------------------------------------------
    -- package -----------------------------------------------------------------

    pkgListH _cnt = do
        withDbc $ \dbconn -> PGS.withTransaction dbconn $ do
            res <- PGS.query_ dbconn
                   "SELECT pname,r.ptime \
                   \FROM (SELECT DISTINCT pname FROM pkgindex) AS i \
                   \LEFT JOIN pname_max_ptime r USING (pname) \
                   \ORDER BY pname"
            tags <- queryAllTagsInv dbconn
            let ents = [ PkgListEntry pn (Map.findWithDefault [] pn tags) (fmap ptime2utc pt)
                       | (pn,pt) <- res ]
            pure $! mkListSlice 0 ents

    llrListH _cnt = do
        withDbc $ \dbconn -> PGS.withTransaction dbconn $ do
            res <- PGS.query_ dbconn
                   "SELECT pname,ptime FROM pname_max_ptime ORDER BY ptime desc,pname"
            let ents = [ PkgListEntry2 pn (ptime2utc pt) | (pn,pt) <- res ]
            pure $! mkListSlice 0 ents

    pkgVerInfoH pkgn = do
        withDbc $ \dbconn -> do
            res <- PGS.query dbconn "SELECT pver, max(prev) FROM pkgindex WHERE pver <> '' AND pname = ? GROUP BY pver" (PGS.Only pkgn)
            let ents = sort [ PkgVerInfoEntry v (fromIntegral (rev :: Int))  "normal" | (v,rev) <- res ]
            pure (PkgVerInfo pkgn ents)


    pkgTagsH pkgn = do
        withDbc $ \dbconn -> do
            res <- PGS.query dbconn "SELECT tagname FROM pname_tag WHERE pname = ? ORDER BY tagname" (PGS.Only pkgn)
            pure (map PGS.fromOnly res)


    pkgLastReport pname = do
        ptimes <- withDbc $ \dbconn -> do
            map fromOnly <$>
              PGS.query dbconn "SELECT ptime FROM pname_max_ptime WHERE pname = ?"
                (Only pname)

        case ptimes of
          []      -> throwServantErr' err404
          ptime:_ -> withDbc $ \dbconn -> queryJobReport dbconn pname ptime

    pkgCellReport :: PkgN -> Text -> AppHandler CellReport
    pkgCellReport pname cellid = do
        let [gv1,pver1] = T.splitOn "-" cellid

        liftIO $ print (pname,gv1,pver1)

        let Just gv = simpleParse (T.unpack gv1)
        -- let crGhcFullVersion = crGhcVersion
        let pver :: Ver
            Just pver = simpleParse (T.unpack pver1)

        ptimes <- withDbc $ \dbconn -> do
            map fromOnly <$>
              PGS.query dbconn "SELECT ptime FROM pname_max_ptime WHERE pname = ?"
                (Only pname)

        case ptimes of
          []      -> throwServantErr' err404
          ptime:_ -> withDbc $ \dbconn -> queryCellReport dbconn (PkgId pname pver) gv ptime

    ----------------------------------------------------------------------------
    -- queue -------------------------------------------------------------------

    queListH _cnt = do
        ents <- withDbc $ \dbconn -> do
            PGS.query_ dbconn "SELECT prio,modified,pname,ptime FROM queue ORDER BY prio desc, modified desc"
        pure $ mkListSlice 0 ents

    quePostH qent = withDbc $ \dbconn -> do
        let pn = qPackageName qent
            pp = qPriority qent

        _ <- PGS.execute dbconn "INSERT INTO queue(pname,prio) \
                                \VALUES (?,?) \
                                \ON CONFLICT (pname) \
                                \DO UPDATE SET prio = EXCLUDED.prio, modified = DEFAULT"
                                (pn,pp)
        pure ()

    queGetH pkgn = do
        ents <- withDbc $ \dbconn -> do
            PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue WHERE pname = ?" (PGS.Only pkgn)
        case ents of
          [] -> throwServantErr' err404
          (e:_) -> pure e

    quePutH pkgn prio = do
        ents <- withDbc $ \dbconn -> do
            PGS.query dbconn "UPDATE queue SET prio = ?, modified = DEFAULT \
                             \WHERE pname = ? \
                             \RETURNING prio,modified,pname,ptime"
                             (prio,pkgn)

        case ents of
          [] -> throwServantErr' err404
          (e:_) -> pure e

    queDelH pkgn = do
        withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "DELETE FROM queue WHERE pname = ?" (PGS.Only pkgn)
            pure ()




withDbc :: (PGS.Connection -> IO a) -> AppHandler a
withDbc act = do
    pool <- gets appDbPool
    liftIO (withResource pool act)

type AppHandler = Handler App App

queryAllTags :: PGS.Connection -> IO (Map TagName [PkgN])
queryAllTags dbconn = do
    res <- PGS.query_ dbconn "SELECT tagname,pname FROM pname_tag"
    pure $ Map.fromListWith (++) [ (tn,[pn]) | (tn,pn) <- res ]

queryAllTagsInv :: PGS.Connection -> IO (Map PkgN [TagName])
queryAllTagsInv dbconn = do
    res <- PGS.query_ dbconn "SELECT tagname,pname FROM pname_tag"
    pure $ Map.fromListWith (++) [ (pn,[tn]) | (tn,pn) <- res ]

is2lbs :: InputStream ByteString -> IO LBS.ByteString
is2lbs s = LBS.fromChunks <$> Streams.toList s


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



whileM_ :: (Monad m) => m Bool -> m ()
whileM_ p = go
  where
    go = do
        x <- p
        if x
        then go
        else pure ()



----------------------------------------------------------------------------

queryCellReport :: PGS.Connection -> PkgId -> Ver -> PkgIdxTs -> IO CellReport
queryCellReport dbconn (PkgId pname pver) gv2 ptime = do
    let crGhcVersion = gv2
        crGhcFullVersion = compilerVer cid
        Just cid = Map.lookup crGhcVersion mgv

    msgs <- PGS.query dbconn
             "SELECT logmsg FROM solution \
             \JOIN iplan_job j USING (jobid) \
             \JOIN iplan_unit u ON (xunitid = ANY(units)) \
             \WHERE logmsg is not null \
             \  AND j.pname = ? AND j.pver = ? AND j.compiler = ? AND ptime = ? \
             \ORDER BY u.ctime ASC"
             (pname, pver, cid, ptime)

    let crLogMsg = T.unlines (map fromOnly msgs)

    pure CellReport{..}
  where -- HACK/FIXME
    m = [ ("7.10", "ghc-7.10.3")
        , ("8.0",  "ghc-8.0.2")
        , ("7.8",  "ghc-7.8.4")
        , ("7.6",  "ghc-7.6.3")
        , ("7.4",  "ghc-7.4.2")
        ]

    mgv :: Map Ver CompilerID
    mgv = Map.fromList [ (k,v) | (k0,v0) <- m
                               , Just k <- [simpleParse k0]
                               , Just v <- [simpleParse v0]
                               ]

queryJobReport :: PGS.Connection -> PkgN -> PkgIdxTs -> IO JobReport
queryJobReport dbconn pname ptime = do
    vrevs <- Map.fromList <$>
             PGS.query dbconn
             "SELECT pver, max(prev) FROM pkgindex \
             \WHERE pver <> '' AND pname = ? AND ptime <= ? \
             \GROUP BY pver" (pname, ptime)

    evaluate (rnf vrevs)

    ipfails <- PGS.query dbconn
               "SELECT compiler,pver FROM solution_fail WHERE pname = ? AND ptime = ?"
               (pname, ptime)

    evaluate (rnf ipfails)

    jobs <- PGS.query dbconn
            "SELECT DISTINCT j.compiler,j.pver,bstatus \
            \FROM iplan_job j JOIN solution USING (jobid) \
            \JOIN iplan_unit ON (xunitid = ANY (units)) \
            \WHERE j.pname = ? AND ptime = ?"
            (pname, ptime)

    evaluate (rnf jobs)

    let ipsols :: Map Ver (Map Ver JobResultType)
        ipsols = Map.map t1 $
                 Map.fromListWith (Map.unionWith mappend) [ (compilerVer k, Map.singleton v [st]) | (k,v,st) <- jobs ]

        t1 :: Map Ver [Maybe IPStatus] -> Map Ver JobResultType
        t1 vst0 = Map.fromList [ (v,st) | (v,st0) <- Map.toList vst0, Just st <- [st2res st0] ]

        st2res :: [Maybe IPStatus] -> Maybe JobResultType
        st2res [] = Just JRTNoIpFail
        st2res xs
          | all (== Nothing) xs               = Nothing
          | all (== Just IPOk) xs             = Just JRTOk
          | any (== Just IPBuildFail) xs      = Just JRTFail
          | any (== Just IPBuildDepsFail) xs  = Just (JRTFailDeps 1)
          | otherwise                         = Just JRTNoIpFail

    let ipfailm :: Map Ver (Map Ver JobResultType)
        ipfailm = Map.fromListWith mappend [ (compilerVer k,Map.singleton v JRTNoIp) | (k,v) <- ipfails ]

        table = Map.unionWith mappend ipfailm ipsols -- TODO: assert non-overlap

    let jrResults = [ JobResult (alterVer (take 2) gv) gv
                      [ JobGhcResult v (Map.findWithDefault 0 v vrevs) ty
                      | (v,ty) <- Map.toList ents ]
                    | (gv, ents) <- Map.toList table ]
        jrModified = ptime2utc ptime

        jrPackageName = pname

    pure JobReport{..}


data JobReport = JobReport
 { jrPackageName :: PkgN
 , jrModified    :: UTCTime
 , jrResults     :: [JobResult]
 } deriving (Eq, Show, Generic)

instance ToJSON   JobReport where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobReport where { parseJSON = myParseJSONCml }

data JobResult = JobResult
  { jrGhcVersion     :: Ver
  , jrGhcFullVersion :: Ver
  , jrGhcResult      :: [JobGhcResult]
  } deriving (Eq, Generic, Show)

instance ToJSON   JobResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobResult where { parseJSON = myParseJSONCml }

data JobGhcResult = JobGhcResult
  { jgrPackageVersion  :: Ver
  , jgrPackageRevision :: Int
  , jgrResult          :: JobResultType
  } deriving (Eq, Generic, Show)

instance ToJSON   JobGhcResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobGhcResult where { parseJSON = myParseJSONCml }

data JobResultType
    = JRTOk
    | JRTNop
    | JRTNoIp
    | JRTNoIpBjLimit Word
    | JRTNoIpFail
    | JRTFail
    | JRTFailDeps Word
    deriving (Eq, Generic, Show)

instance ToJSON   JobResultType where { toJSON = J.genericToJSON jobResOpts }
instance FromJSON JobResultType where { parseJSON = J.genericParseJSON jobResOpts }

data CellReport = CellReport
  { crGhcVersion     :: Ver
  , crGhcFullVersion :: Ver
  , crLogMsg         :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON CellReport where
    toJSON CellReport{..} =
        J.object [ "ghcVersion" J..= crGhcVersion
                 , "ghcFullVersion" J..= crGhcFullVersion
                 , "resultA" J..=
           J.object [ "result" J..=
              J.object [ "fail" J..= crLogMsg ] ] ]

           -- [ "ghcVersion" J..= crGhcVersion ]

jobResOpts :: J.Options
jobResOpts = J.defaultOptions { J.sumEncoding = J.ObjectWithSingleField
                              , J.constructorTagModifier = labelModCml }
  where
    labelModCml = uncap . drop 3

    uncap [] = []
    uncap (c:cs) = toLower c : cs
