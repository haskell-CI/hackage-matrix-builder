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

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
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
import           Log
import           PkgId
import qualified PkgIdxTsSet
import           System.IO
import           WorkerApi
import           WorkerApi.Client

pkgIdxTupleToDB :: PkgIdxTuple -> (Text,Text,Int,Int,Text,Int)
pkgIdxTupleToDB PkgIdxTuple{..} = (pn, ver, rev, t, pitOwner, pitOwnerId)
  where
    pn  = tdisplay pitName
    ver = maybe "" tdisplay $ pitVer
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


logDebugIns :: Text -> IO Int64 -> IO ()
logDebugIns lbl act = do
  cnt <- act
  logDebug (mconcat ["new ", lbl, " entries: ", tshow cnt])

registerPkgIds :: PGS.Connection -> Set PkgId -> IO ()
registerPkgIds dbconn pids = do
    logDebugIns "pkgname" $
      PGS.executeMany dbconn "INSERT INTO pkgname (pname) VALUES (?) EXCEPT SELECT pname FROM pkgname" [ Only n | n <- Set.toList pnames ]

    logDebugIns "version" $
      PGS.executeMany dbconn "INSERT INTO version (pver) VALUES (?) EXCEPT SELECT pver FROM version" [ Only v | v <- Set.toList pvers ]

    logDebugIns "pkgver" $
      PGS.executeMany dbconn "INSERT INTO pkgver (pname,pver) VALUES (?,?) ON CONFLICT DO NOTHING" [ (n,v) | PkgId n v <- Set.toList pids ]
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
              mapM_ (logDebug . tshow) pids
              registerPkgIds dbconn pids


performIndexUpdate :: PGS.Connection -> IO ()
performIndexUpdate dbconn = do
    t0 <- getPOSIXTime
    logDebug "reading db index"
    oldents <- (evaluate . force . HS.fromList) =<< PGS.query_ dbconn "SELECT pname,pver,prev FROM pkgindex"
    t1 <- getPOSIXTime
    logDebug $ "reading db index took " <> tshow (t1-t0)

    logDebug "reading index..."
    idxtups <- readIndexTuples indexTar
    pindex' <- evaluate . force . filter (\(a,b,c,_,_,_) -> not (HS.member (PkgIdxKey a b c) oldents) && b /= "") . map pkgIdxTupleToDB $ idxtups
    let newPkgIds = Set.fromList [ PkgId pn pv | (pn0,pv0,_,_,_,_) <- pindex'
                                               , Just pn <- [simpleParse (T.unpack pn0)]
                                               , Just pv <- [simpleParse (T.unpack pv0)]
                                               ]

    t2 <- getPOSIXTime
    logDebug $ "reading index took " <> tshow (t2-t1)

    logDebug "registering new db entries..."

    registerPkgIds dbconn newPkgIds

    logDebugIns "owner" $
      PGS.executeMany dbconn "INSERT INTO owner (owner_id,name) VALUES (?,?) ON CONFLICT DO NOTHING" [(oi,o) | (_,_,_,_,o,oi) <- pindex']

    logDebugIns "idxstate" $
      PGS.executeMany dbconn "INSERT INTO idxstate (ptime) VALUES (?) ON CONFLICT DO NOTHING" [Only t | (_,_,_,t,_,_) <- pindex']

    logDebugIns "pkgindex" $
      PGS.executeMany dbconn "INSERT INTO pkgindex (pname,pver,prev,ptime,powner,powner_id) VALUES (?,?,?,?,?,?) ON CONFLICT DO NOTHING" pindex'

    t3 <- getPOSIXTime
    logDebug $ "updating db took " <> tshow (t3-t2)

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
        logDebug "opening new dbconn"
        PGS.connect ci

    killConn c = do
        logDebug "closing a dbconn"
        PGS.close c
