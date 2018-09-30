{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

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

module Main (main) where

import           Prelude.Local

import           Control.Monad.Except
import qualified Data.HashSet                     as HS
import           Data.Pool
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as PGS
import           Database.PostgreSQL.Simple.Types (Only (..))
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   managerResponseTimeout,
                                                   newManager,
                                                   responseTimeoutNone)
import           Servant.Client

-- local modules
import qualified Controller.Cli                   as Cli
import qualified Controller.Compute               as Comp
import           Controller.Config
import qualified Controller.Scheduler             as Sched
import qualified Controller.WebSvc                as WebSvc
import           IndexHelper
import           PkgId
import qualified PkgIdxTsSet
import           System.IO
import           WorkerApi
import           WorkerApi.Client

pkgIdxTupleToDB :: PkgIdxTuple -> (Text,Text,Int,Int,Text)
pkgIdxTupleToDB PkgIdxTuple{..} = (pn, ver, rev, t, pitOwner)
  where
    pn  = T.pack $ display pitName
    ver = maybe "" (T.pack . display) $ pitVer
    rev = fromIntegral pitRev
    t   = unPkgIdxTs pitTime

data PkgIdxKey = PkgIdxKey !Text !Text !Int
               deriving (Generic,Eq,Ord)

instance Hashable PkgIdxKey
instance NFData   PkgIdxKey
instance PGS.FromRow PkgIdxKey

{-
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

-}

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
        res <- runExceptT $ runClientM' manager wuri $ listPkgDbGlobal gv
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


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    opts <- Cli.getOpts
    cconf <- readConfig "controller.cfg"

    case opts of
      Cli.Init -> do
          bracket mkConn killConn (\c -> initWorkers c (ccWorkers cconf))
          exitSuccess

      Cli.Update -> do
          bracket mkConn killConn performIndexUpdate
          exitSuccess

      Cli.Compute -> do
          bracket mkConn killConn Comp.runCompute
          exitSuccess

      Cli.Scheduler -> do
          Sched.runScheduler ci (ccWorkers cconf)
          exitSuccess

      Cli.WebServer -> do
          appDbPool <- createPool mkConn killConn 1 10.5 4
          appPkgLstCache <- newMVar (WebSvc.PkgLstCache (PkgIdxTs 0) mempty)
          appPkgIdxTsCache <- newMVar PkgIdxTsSet.empty
          let app = WebSvc.App{..}

          WebSvc.runController app (ccPort cconf)

          exitSuccess

  where
    -- TODO: read parts of db-info from config
    ci = PGS.ConnectInfo "" 0 "" "" "matrix"

    mkConn = do
        putStrLn "opening new dbconn"
        PGS.connect ci

    killConn c = do
        putStrLn "closing a dbconn"
        PGS.close c
