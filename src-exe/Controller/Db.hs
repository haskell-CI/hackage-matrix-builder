{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- WARNING: the code that follows will make you cry;
--          a safety pig is provided below for your benefit.
--
--                           _
--   _._ _..._ .-',     _.._(`))
--  '-. `     '  /-._.-'    ',/
--     )         \            '.
--    / _    _    |             \
--   |  a    a    /              |
--   \   .-.                     ;
--    '-('' ).-'       ,'       ;
--       '-;           |      .'
--          \           \    /
--          | 7  .__  _.-\   \
--          | |  |  ``/  /`  /
--         /,_|  |   /,_/   /
--            /,_/      '`-'
--

module Controller.Db where

import           Prelude.Local

import qualified Data.Aeson                           as J
import qualified Data.ByteString.Char8                as BS
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types     (Only (..))
import qualified Database.PostgreSQL.Simple.Types     as PGS

import           Controller.Api
import           PkgId
import           PlanJson                             as PJ

-- TODO
type HcID = PkgId

-- for PG's benefit
newtype UUIDs = UUIDs { unUUIDs :: [UUID] }
    deriving (Eq,Generic,Show)

ptime2utc :: PkgIdxTs -> UTCTime
ptime2utc = posixSecondsToUTCTime . fromIntegral . unPkgIdxTs

-- we assume that 'show' doesn't use non-latin1 code-points
toUUID :: Show a => a -> UUID
toUUID = uuidHash . BS.pack . show

instance ToField UUIDs where
    toField (UUIDs xs) = Many [toField (PGS.PGArray xs), Plain "::uuid[]" ]

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
        go _                  = error ("FromField(IPStatus) " ++ show mdata)


data DB_iplan_unit = DB_iplan_unit UUID UnitID HcID PIType PkgN Ver J.Value (Maybe IPStatus) (Maybe Text) (Maybe NominalDiffTime)
                   deriving (Show,Generic)

db_iplan_unit_insert :: PGS.Query
db_iplan_unit_insert = "INSERT INTO iplan_unit(xunitid,unitid,compiler,pkind,pname,pver,flags,bstatus,logmsg,dt) VALUES (?,?,?,?,?,?,?,?,?,?)"

instance PGS.ToRow DB_iplan_unit


----

-- | Represents the kind of dependency (build-tool-depends vs build-depends).
--
-- Note: this is not fully accurate yet; in case of libraries a
-- package may contain more than one library; likewise a package may
-- contain more than one executable; we will need to extend this at
-- some point;
data DepKind = DepKindLib | DepKindExe
             deriving (Show,Eq)

-- currently represented as boolean in SQL
instance ToField DepKind where
    toField DepKindLib = toField False
    toField DepKindExe = toField True

-- currently represented as boolean in SQL
instance FromField DepKind where
    fromField f mdata = b2dk <$> fromField f mdata
      where
        b2dk False = DepKindLib
        b2dk True  = DepKindExe

data DB_iplan_comp_dep = DB_iplan_comp_dep {- parent :: -} UUID {- cname :: -} CompName  {- isExeDep :: -} DepKind {- child :: -} UUID
                       deriving (Show,Generic)

instance PGS.ToRow DB_iplan_comp_dep

db_iplan_comp_dep_insert :: PGS.Query
db_iplan_comp_dep_insert = "INSERT INTO iplan_comp_dep(parent,cname,isExeDep,child) VALUES (?,?,?,?)"

----

data DB_iplan_job = DB_iplan_job UUID PkgN Ver HcID J.Value UUIDs
                  deriving (Generic,Show)

db_iplan_job_insert :: PGS.Query
db_iplan_job_insert = "INSERT INTO iplan_job(jobid,pname,pver,compiler,plan,units) VALUES (?,?,?,?,?,?)"

instance PGS.ToRow DB_iplan_job

----------------------------------------------------------------------------
-- queries

queryQEntries :: PGS.Connection -> IO [QEntry]
queryQEntries dbconn =
    PGS.query_ dbconn "SELECT prio,modified,pname,ptime FROM queue ORDER BY prio desc, modified desc, ptime desc, pname asc"

deleteQEntry :: PGS.Connection -> QEntry -> IO ()
deleteQEntry dbconn QEntry{..} =
    void $ PGS.execute dbconn "DELETE FROM queue WHERE pname = ? AND ptime = ?" (qPackageName, qIdxState)

-- pname,prio,modified,ptime
-- data DB_queue = DB_queue PkgN Int UTCTime PkgIdxTs
--               deriving (Show,Generic)
-- instance PGS.FromRow DB_queue


doNothing :: PGS.Query -> PGS.Query
doNothing = flip mappend " ON CONFLICT DO NOTHING"

queryJobExists :: PGS.Connection -> UUID -> IO Bool
queryJobExists dbconn jid = do
    PGS.query dbconn "SELECT EXISTS (SELECT 1 FROM iplan_job WHERE jobid = ?)" (Only jid) >>= \case
        [Only exists] -> pure exists
        _ -> fail "queryJobExists: the impossible happened"


-- | Minimal specification of a single build job
data JobSpec = JobSpec PkgId PkgIdxTs CompilerID
             deriving (Eq,Ord,Show)

queryNextJobTask :: PGS.Connection -> [CompilerID] -> PkgN -> PkgIdxTs -> IO (Maybe JobSpec)
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

    pure $! case pvgvs of
      [] -> Nothing
      _  -> let (pver,gv) = maximumBy (comparing (first pverOrd)) pvgvs
            in Just $! JobSpec (PkgId pname pver) ptime gv

----------------------------------------------------------------------------

queryCellReport :: PGS.Connection -> PkgId -> Ver -> PkgIdxTs -> IO CellReport
queryCellReport dbconn (PkgId pname pver) ghc_ui_ver ptime = do
    let crGhcVersion = ghc_ui_ver

    -- TODO: error handling
    [Only cid] <- PGS.query dbconn "SELECT compiler FROM hscompiler WHERE ui_ver = ?" (Only ghc_ui_ver)

    let crGhcFullVersion = compilerVer cid

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


queryPkgReport :: PGS.Connection -> PkgN -> PkgIdxTs -> IO PkgIdxTsReport
queryPkgReport dbconn pname ptime = do
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

    let ipsols0 :: Map Ver (Map GhcVer [Maybe IPStatus])
        ipsols0 = Map.fromListWith (Map.unionWith mappend)
                                   [ (v, Map.singleton (compilerVer k) [st]) | (k,v,st) <- jobs ]

        ipsols :: Map Ver (Map GhcVer CellReportSummary)
        ipsols = Map.map t1 ipsols0

        t1 :: Map GhcVer [Maybe IPStatus] -> Map GhcVer CellReportSummary
        t1 vst0 = Map.fromList [ (v,st) | (v,st0) <- Map.toList vst0
                                        , Just st <- [st2res st0]
                                        ]

        st2res :: [Maybe IPStatus] -> Maybe CellReportSummary
        st2res [] = Just noipFailCRS
        st2res xs
          | all (== Nothing) xs               = Nothing
          | all (== Just IPOk) xs             = Just bokCRS
          | any (== Just IPBuildFail) xs      = Just bfailCRS
          | any (== Just IPBuildDepsFail) xs  = Just bdfailCRS
          | otherwise                         = Just noipFailCRS

    let ipfailm, table :: Map Ver (Map GhcVer CellReportSummary)
        ipfailm = Map.fromListWith mappend [ (v,Map.singleton (compilerVer k) noipCRS)
                                           | (k,v) <- ipfails
                                           ]

        table = Map.unionWith mappend ipfailm ipsols -- TODO: assert non-overlap

    let gvs :: [GhcVer]
        gvs = Set.toDescList $ mconcat (map Map.keysSet (Map.elems table))

    evaluate (rnf gvs)

    let pitrPkgversions = Map.map m2l table

        m2l :: Map GhcVer CellReportSummary -> [CellReportSummary]
        m2l m = map (\gv -> Map.findWithDefault naCRS gv m) gvs

    evaluate (rnf pitrPkgversions)

    let pitrIdxstate = ptime
        pitrPkgname  = pname

        pitrHcversions = map mkGhcCompilerID gvs

    pure PkgIdxTsReport{..}
  where
    naCRS = CellReportSummary Nothing Nothing Nothing Nothing Nothing Nothing

    noipCRS     = naCRS { crsT = Just CRTpf }
    noipFailCRS = naCRS { crsT = Just CRTpf, crsPerr = Just True }

    bokCRS      = naCRS { crsT = Just CRTse, crsBok    = Just 1 }
    bfailCRS    = naCRS { crsT = Just CRTse, crsBfail  = Just 1 }
    bdfailCRS   = naCRS { crsT = Just CRTse, crsBdfail = Just 1 }


queryCellReport2 :: PGS.Connection -> PkgIdxTs -> PkgId -> CompilerID -> IO CellReportDetail
queryCellReport2 dbconn crdIdxstate (PkgId crdPkgname crdPkgversion) crdHcversion = do
    -- [Only cid] <- PGS.query dbconn "SELECT compiler FROM hscompiler WHERE ui_ver = ?" (Only ghc_ui_ver)

    pferr <- PGS.query dbconn
             "SELECT solvererr \
             \FROM solution_fail sf \
             \WHERE sf.pname = ? AND sf.pver = ? AND sf.compiler = ? AND sf.ptime = ?" -- PK
             (crdPkgname, crdPkgversion, crdHcversion, crdIdxstate)

    case pferr of
      (Only err:_) -> do
          let crdType = CRTpf
              crdSolverErr = Just err
              crdUnits     = Nothing
          pure CellReportDetail{..}

      [] -> do -- no plan-fail (yet)
          let crdSolverErr = Nothing

          jobs <- PGS.query dbconn
                  "SELECT DISTINCT jobid,xunitid,bstatus \
                  \FROM iplan_job j JOIN solution USING (jobid) \
                  \JOIN iplan_unit ON (xunitid = ANY (units)) \
                  \WHERE j.pname = ? AND j.pver = ? AND j.compiler = ? AND ptime = ?"
                  (crdPkgname, crdPkgversion, crdHcversion, crdIdxstate)

          let jobs' = Map.elems $ Map.fromListWith (<>)
                      [ (jobid,Map.singleton xunitid (ips2txt mbstatus))
                      | (jobid,xunitid,mbstatus) <- (jobs :: [(UUID, UUID, Maybe IPStatus)]) ]

          if null jobs'
          then do
              let crdType = CRTna
                  crdUnits = Nothing
              pure CellReportDetail{..}
          else do
              let crdType = CRTse
                  crdUnits = Just jobs'
              pure CellReportDetail{..}
  where
    ips2txt :: Maybe IPStatus -> Text
    ips2txt Nothing                = ""
    ips2txt (Just IPOk)            = "ok"
    ips2txt (Just IPBuildFail)     = "bfail"
    ips2txt (Just IPBuildDepsFail) = "bdfail"
