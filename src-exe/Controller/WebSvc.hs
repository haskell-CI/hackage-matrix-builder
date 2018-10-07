{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

module Controller.WebSvc where

import           Prelude.Local

import           Control.Monad.Except
import           Control.Monad.State
import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.Aeson                       as J
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Foldable                    as F
import           Data.Function                    (on)
import qualified Data.Map.Strict                  as Map
import           Data.Pool
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Database.PostgreSQL.Simple       as PGS
import           Database.PostgreSQL.Simple.Types (Only (..))
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   managerResponseTimeout,
                                                   newManager,
                                                   responseTimeoutNone)
import qualified Network.HTTP.Types.Header        as HTTP
import qualified Network.HTTP.Types.Status        as HTTP
import           Servant
import           Servant.Client                   (ServantError (..))
import qualified Servant.Client                   as Serv
import           Snap.Core
import           Snap.Http.Server                 (defaultConfig)
import qualified Snap.Http.Server.Config          as Snap
import           Snap.Snaplet
import qualified Snap.Util.CORS                   as Snap
import qualified Snap.Util.FileServe              as Snap
import qualified System.IO.Streams                as Streams

-- local modules
import           Controller.Api
import           Controller.Db
import           HackageApi
import           HackageApi.Client
import           Log
import           PkgId
import           PkgIdxTsSet                      (PkgIdxTsSet)
import qualified PkgIdxTsSet

-- | See 'fetchPkgLstCache'
data PkgLstCache = PkgLstCache !PkgIdxTs !(Vector PkgN)

data App = App
  { appDbPool        :: Pool PGS.Connection
  , appPkgLstCache   :: MVar PkgLstCache   -- ^ see 'fetchPkgLstCache'
  , appPkgIdxTsCache :: MVar PkgIdxTsSet -- ^ see 'fetchPkgIdxTsCache'
  }

newtype ETag = ETag ByteString

etagFromPkgIdxTs :: Maybe PkgIdxTs -> Int -> ETag
etagFromPkgIdxTs mis sz
  | sz == 0   = ETag (bb2bs (i2bb ts))
  | otherwise = ETag (bb2bs (i2bb ts <> BB.char8 ':' <> i2bb sz))
  where
    ts = maybe 0 unPkgIdxTs mis
    bb2bs = LBS.toStrict . BB.toLazyByteString
    i2bb = BB.wordHex . fromIntegral

-- Quick'n'dirty helper; Try to avoid this
etagFromHashable :: Hashable a => a -> ETag
etagFromHashable = ETag . i2bs . hash
  where
    i2bs :: Int -> ByteString
    i2bs = LBS.toStrict . BB.toLazyByteString . BB.wordHex . fromIntegral

runController :: App -> Word16 -> IO ()
runController !app port =
    serveSnaplet conf initApp
  where
    conf = Snap.setPort (fromIntegral port) $
           Snap.setBind "127.0.0.1" $
           Snap.setProxyType Snap.xForwardedFor $
           defaultConfig

    initApp :: SnapletInit App App
    initApp = makeSnaplet "matrix-controller" "Matrix CI controller" Nothing $ do
        addRoutes [("/api/swagger.json",apiSwaggerJsonHandler)
                  ,("/api/",      apiHandler)

                  ,("/package/",  hashRedir)
                  ,("/packages/", hashRedir)
                  ,("/user/",     hashRedir)
                  ,("/users/",    hashRedir)
                  ,("/latest/",   hashRedir)

                  ,("/ng/",       uiHandlerNg)
                  ,("/",          uiHandler)
                  ]
        return app

    apiSwaggerJsonHandler :: AppHandler ()
    apiSwaggerJsonHandler = Snap.applyCORS Snap.defaultOptions $ do
        modifyResponse $ setContentType "application/json"
        writeLBS (J.encode swaggerDoc)

    apiHandler :: AppHandler ()
    apiHandler = Snap.applyCORS Snap.defaultOptions $ serveSnap controllerApi server

    uiHandler :: AppHandler ()
    uiHandler = Snap.serveDirectory "ui.v2"

    uiHandlerNg :: AppHandler ()
    uiHandlerNg = Snap.serveDirectory "ui.v3"

    controllerApi :: Proxy (ControllerApi AppHandler)
    controllerApi = Proxy

    -- | Redirect to hash-routing urlpath
    hashRedir :: MonadSnap m => m ()
    hashRedir = do
        uri <- withRequest (pure . rqURI)
        redirect' ("/#" <> uri) 307

mkListSlice :: Word -> [a] -> ListSlice a
mkListSlice ofs xs = ListSlice ofs (fromIntegral $ length xs) xs

server :: Server (ControllerApi AppHandler) '[] AppHandler
server = tagListH
    :<|> tagSetH
    :<|> tagDelH

    :<|> pkgListH
    :<|> llrListH
    :<|> pkgVerInfoH
    :<|> pkgTagsH
    :<|> pkgLastReport
    :<|> pkgLastCellReport
    :<|> pkgIdxStReport
    :<|> pkgIdxStCellReport

    :<|> queListH
    :<|> quePostH
    :<|> queGetH
    :<|> quePutH
    :<|> queDelH

    :<|> usrListH

    -- v2
    :<|> idxStatesH
    :<|> idxStatesLatestH

    :<|> packagesH
    :<|> packagesTagsH
    :<|> reportsStarH
    :<|> reportsH
    :<|> reportsIdxStH
    :<|> reportsIdxStCellH
    :<|> packagesHistoryH

    :<|> unitsIdH
    :<|> unitsIdDepsH

    :<|> tagsH
    :<|> tagsGetH
    :<|> tagsPutH
    :<|> tagsDelH

    :<|> queueGetH
    :<|> queueGetPkgH
    :<|> queueGetPkgIdxStH
    :<|> queuePutPkgIdxStH
    :<|> queueDelPkgIdxStH
    :<|> usersListH
  where

    needAuth :: AppHandler a -> AppHandler a
    needAuth h = do
        rq <- getRequest
        let mauth = getHeader "Authorization" rq

        case fmap BS.words mauth of
          Just ["Basic",cred] -- FIXME; quick hack
            | s3cr3t == SHA256.hash cred -> pure ()
            | otherwise -> do
                  Log.logError ("bad credentials: " <> tshow cred)
                  throwBasicAuth401 "Hackage Matrix"
          _ -> throwBasicAuth401 "Hackage Matrix"
        h
      where
        s3cr3t = "w/\207\193f\ENQ\248\DLE\202\160.\181\203<\188!\161U\147\232\DC4R\151\152g\188\218d\NAK\SI\150\172"

    -- Quick'n'dirty etag/if-none-match logic
    -- NB: if-none-match header value is not properly parsed and may
    -- easily not be detected if it contains trailing whitespace etc
    doEtag :: AppHandler (ETag, AppHandler a) -> AppHandler a
    doEtag h = do
        rq <- getRequest
        (ETag etag', cont) <- h
        let qetag = mconcat ["\"", etag', "\""]
        let mNoneMatch = getHeader "If-None-Match" rq
        when (Just qetag == mNoneMatch) $
            throwNotModified304 qetag
        modifyResponse $ setHeader "ETag" qetag
        cont

    doEtagFoldableGet :: (Foldable f, Hashable a) => AppHandler (f a) -> AppHandler (f a)
    doEtagFoldableGet h0 = do
        xs <- h0
        let etag = etagFromHashable (F.toList xs)
        doEtag $ pure (etag, pure xs)

    doEtagHashableGet :: Hashable a => AppHandler a -> AppHandler a
    doEtagHashableGet h0 = do
        res <- h0
        let etag = etagFromHashable res
        doEtag $ pure (etag, pure res)

    ----------------------------------------------------------------------------
    -- tag ---------------------------------------------------------------------

    tagListH _cnt = do
        withDbc $ \dbconn -> do
            tags <- queryAllTags dbconn
            pure $ mkListSlice 0 [ TagListEntry tn pns | (tn,pns) <- Map.toList tags ]

    tagSetH, tagDelH :: TagName -> PkgN -> AppHandler ()
    tagSetH tagn pkgn = needAuth $ withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "INSERT INTO pname_tag (tagname,pname) values (?,?) ON CONFLICT DO NOTHING" (tagn,pkgn)
            pure ()

    tagDelH tagn pkgn = needAuth $ withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "DELETE FROM pname_tag WHERE tagname = ? AND pname = ?" (tagn,pkgn)
            pure ()

    ----------------------------------------------------------------------------
    -- package -----------------------------------------------------------------

    pkgListH :: Maybe Word -> AppHandler (ListSlice PkgListEntry)
    pkgListH mcnt = do
        withDbc $ \dbconn -> PGS.withTransaction dbconn $ do
            res <- case mcnt of
                Nothing -> PGS.query_ dbconn
                            "SELECT pname,r.ptime \
                            \FROM (SELECT DISTINCT pname FROM pkgindex) AS i \
                            \LEFT JOIN pname_max_ptime r USING (pname) \
                            \ORDER BY pname"
                Just cnt -> PGS.query dbconn
                             "SELECT pname,r.ptime \
                             \FROM (SELECT DISTINCT pname FROM pkgindex) AS i \
                             \LEFT JOIN pname_max_ptime r USING (pname) \
                             \ORDER BY pname \
                             \LIMIT ?"
                             (Only cnt)

            tags <- queryAllTagsInv dbconn
            let ents = [ PkgListEntry pn (Map.findWithDefault mempty pn tags) (fmap ptime2utc pt)
                       | (pn,pt) <- res ]
            pure $! mkListSlice 0 ents

    llrListH :: Maybe Word -> AppHandler (ListSlice PkgListEntry2)
    llrListH mcnt = do
        withDbc $ \dbconn -> PGS.withTransaction dbconn $ do
            res <- case mcnt of
                     Nothing  -> PGS.query_ dbconn
                                  "SELECT pname,ptime FROM pname_max_ptime ORDER BY ptime desc,pname"
                     Just cnt -> PGS.query dbconn
                                  "SELECT pname,ptime FROM pname_max_ptime ORDER BY ptime desc,pname LIMIT ?"
                                  (Only cnt)

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

    pkgIdxStReport pname ptime =
        withDbcGuard (pkgnExists pname) $ \dbconn -> queryJobReport dbconn pname ptime

    pkgLastCellReport :: PkgN -> Text -> AppHandler CellReport
    pkgLastCellReport pname cellid = do
        let [gv1,pver1] = T.splitOn "-" cellid

        logDebugShow (pname,gv1,pver1)

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

    pkgIdxStCellReport :: PkgN -> PkgIdxTs -> Text -> AppHandler CellReport
    pkgIdxStCellReport pname ptime cellid = do
        let [gv1,pver1] = T.splitOn "-" cellid

        logDebugShow (pname,ptime,gv1,pver1)

        let Just gv = simpleParse (T.unpack gv1)
        -- let crGhcFullVersion = crGhcVersion
        let pver :: Ver
            Just pver = simpleParse (T.unpack pver1)

        withDbcGuard (pkgnExists pname) $ \dbconn -> queryCellReport dbconn (PkgId pname pver) gv ptime

    ----------------------------------------------------------------------------
    -- v2/idxstates-------------------------------------------------------------

    idxStatesH :: Maybe PkgIdxTs -> Maybe PkgIdxTs -> AppHandler PkgIdxTsSet

    idxStatesH mlb mub = doEtag $ do
        pits0 <- fetchPkgIdxTsCache

        let pits1 = case mlb of
                     Nothing -> pits0
                     Just lb -> case PkgIdxTsSet.lookupIndexGE lb pits0 of
                                  Nothing      -> PkgIdxTsSet.empty
                                  Just (lbi,_) -> PkgIdxTsSet.drop lbi pits0

            pits = case mub of
                     Nothing -> pits1
                     Just ub -> case PkgIdxTsSet.lookupIndexLE ub pits1 of
                                  Nothing      -> PkgIdxTsSet.empty
                                  Just (ubi,_) -> PkgIdxTsSet.take (ubi+1) pits1

        let etag = etagFromPkgIdxTs (PkgIdxTsSet.findMax pits) (PkgIdxTsSet.size pits)
        return (etag, pure pits)

    idxStatesLatestH :: AppHandler PkgIdxTs
    idxStatesLatestH = doEtag $ do
        [Only is] <- withDbc $ \dbconn -> PGS.query_ dbconn "SELECT max(ptime) FROM idxstate"
        let etag = etagFromPkgIdxTs (Just is) 0
        pure (etag, pure is)

    ----------------------------------------------------------------------------
    -- v2/packages -------------------------------------------------------------

    packagesH :: AppHandler (Vector PkgN)
    packagesH = doEtag $ do
        PkgLstCache is plst <- fetchPkgLstCache
        let etag = etagFromPkgIdxTs (Just is) (V.length plst)
        return (etag, pure plst)

    packagesTagsH :: PkgN -> AppHandler (Set TagName)
    packagesTagsH pkgn = doEtagFoldableGet $ withDbcGuard (pkgnExists pkgn) $ \dbconn -> do
        res <- PGS.query dbconn "SELECT tagname FROM pname_tag WHERE pname = ? ORDER BY tagname" (PGS.Only pkgn)
        pure (Set.fromList $ map PGS.fromOnly res)

    packagesHistoryH :: PkgN -> AppHandler (Vector PkgHistoryEntry)
    packagesHistoryH pkgn = doEtag $ do
        xs <- withDbcGuard (pkgnExists pkgn) $ \dbconn ->
          PGS.query dbconn "SELECT ptime,pver,prev,powner FROM pkgindex WHERE pname = ? ORDER BY (ptime,pver,prev,powner)" (PGS.Only pkgn)
        let res = V.fromList xs
            PkgHistoryEntry is _ _ _ = V.last res -- TODO: is res guarnateed to be non-empty?
            etag = etagFromPkgIdxTs (Just is) (V.length res)
        return (etag, pure res)

    reportsStarH :: AppHandler (Map PkgN PkgIdxTs)
    reportsStarH = doEtagHashableGet $ do -- TODO/FIXME: this is *slow*
        xs <- withDbc $ \dbconn -> PGS.query_ dbconn "SELECT pname,ptime FROM pname_max_ptime"
        pure (Map.fromList xs)

    reportsH :: PkgN -> AppHandler (Set PkgIdxTs)
    reportsH pkgn = doEtagFoldableGet $ withDbcGuard (pkgnExists pkgn) $ \dbconn -> do
      {- old (slow) query before we had `pname_ptimes`

        ptimes1 <- PGS.query dbconn "SELECT DISTINCT ptime FROM solution_fail WHERE pname = ?" (PGS.Only pkgn)
        ptimes2 <- PGS.query dbconn "SELECT DISTINCT ptime FROM iplan_job JOIN solution USING (jobid) WHERE pname = ?" (PGS.Only pkgn)
        pure (Set.fromList (map fromOnly ptimes1) <>
              Set.fromList (map fromOnly ptimes2))
      -}
      ptimes <- PGS.query dbconn "SELECT ptime FROM pname_ptimes WHERE pname = ?" (PGS.Only pkgn)
      pure (Set.fromList (map fromOnly ptimes))


    reportsIdxStH :: PkgN -> PkgIdxTs -> AppHandler PkgIdxTsReport
    reportsIdxStH pkgn idxstate = doEtagHashableGet $ do
        res <- withDbcGuard (pkgnExists pkgn) $ \dbconn -> queryPkgReport dbconn pkgn idxstate
        when (null (pitrHcversions res) && null (pitrPkgversions res)) $
            throwServantErr' err404
        pure res

    reportsIdxStCellH :: PkgN -> PkgIdxTs -> Ver -> CompilerID -> AppHandler CellReportDetail
    reportsIdxStCellH pname ptime pver cid = doEtagHashableGet $ withDbcGuard (pkgnExists pname) $ \dbconn ->
        queryCellReport2 dbconn ptime (PkgId pname pver) cid

    pkgnExists :: PkgN -> PGS.Connection -> IO Bool
    pkgnExists pkgn dbconn = do
        [Only ex] <- PGS.query dbconn "SELECT EXISTS (SELECT FROM pkgname WHERE pname = ?)" (PGS.Only pkgn)
        pure ex

    pkgIdxTsExists :: PkgIdxTs -> PGS.Connection -> IO Bool
    pkgIdxTsExists ptime dbconn = do
        [Only ex] <- PGS.query dbconn "SELECT EXISTS (SELECT FROM idxstate WHERE ptime = ?)" (PGS.Only ptime)
        pure ex

    ----------------------------------------------------------------------------
    -- units v2 --------------------------------------------------------------

    -- UUID UnitID HcID PIType PkgN Ver J.Value (Maybe IPStatus) (Maybe Text) (Maybe NominalDiffTime)
    -- iplan_unit(xunitid,unitid,compiler,pkind,pname,pver,flags,bstatus,logmsg,dt)

    unitsIdH :: UUID -> AppHandler UnitIdInfo
    unitsIdH xunitid = doEtagHashableGet $ do
        (uiiHcver,uiiPkgname,uiiPkgver,uiiStatus,uiiLogmsg) <- headOr404M $ withDbc $ \dbconn -> do
            PGS.query dbconn "SELECT compiler,pname,pver,bstatus,logmsg FROM iplan_unit WHERE xunitid = ?" (Only xunitid)

        (libDeps,exeDeps) <- withDbc $ \dbconn -> do
            ld <- PGS.query dbconn "SELECT cname,child FROM iplan_comp_dep WHERE parent = ? AND not isexedep" (Only xunitid)
            ed <- PGS.query dbconn "SELECT cname,child FROM iplan_comp_dep WHERE parent = ? AND isexedep" (Only xunitid)
            pure (ld,ed)

        let uiiLibDeps = Map.fromListWith (<>) [ (cname,Set.singleton child) | (cname,child) <- libDeps ]

            uiiExeDeps | null exeDeps = Nothing
                       | otherwise    = Just $! Map.fromListWith (<>) [ (cname,Set.singleton child) | (cname,child) <- exeDeps ]

        let uiiId = xunitid
        pure UnitIdInfo{..}

    unitsIdDepsH :: UUID -> AppHandler (Map UUID UnitIdTree)
    unitsIdDepsH xunitid = doEtagHashableGet $ do
        rows <- withDbc $ \dbconn -> do
            PGS.query dbconn
                "WITH RECURSIVE t(xunitid) AS ( \
                    \VALUES (?::uuid) \
                    \UNION \
                    \SELECT d.child FROM iplan_comp_dep d, t WHERE d.parent = t.xunitid) \
                  \SELECT t.xunitid,u.pname,u.pver,u.bstatus,d.isexedep,d.cname,d.child \
                    \FROM t JOIN iplan_unit u USING (xunitid) JOIN iplan_comp_dep d ON (xunitid = parent) \
                    \ORDER BY t.xunitid" (Only xunitid)

        when (null rows) $
            throwServantErr' err404

        let grows = map regrp $ groupBy ((==) `on` view _1)
                    (rows :: [(UUID, PkgN, Ver, Maybe IPStatus, Bool, Text, UUID)])

        pure $ Map.fromListWith (error "unitsIdDepsH") grows
      where
        regrp :: [(UUID, PkgN, Ver, Maybe IPStatus, Bool, Text, UUID)] -> (UUID,UnitIdTree)
        regrp [] = error "impossible"
        regrp rows@((xid, pname, pver, bstatus, _, _, _):_)
          = (xid, UnitIdTree { uitPkgname = pname
                             , uitPkgver  = pver
                             , uitStatus  = bstatus
                             , uitLibDeps = ldeps'
                             , uitExeDeps = if null edeps' then Nothing else Just edeps'
                             })
          where
            (edeps, ldeps) = partition (view _5) rows
            ldeps' = Map.fromListWith mappend [ (r ^. _6, Set.singleton (r ^. _7)) | r <- ldeps ]
            edeps' = Map.fromListWith mappend [ (r ^. _6, Set.singleton (r ^. _7)) | r <- edeps ]

    ----------------------------------------------------------------------------
    -- tags v2 --------------------------------------------------------------

    tagsH :: Bool -> AppHandler TagsInfo
    tagsH False = TagsInfo <$> doEtagFoldableGet (withDbc queryAllTagnames)
    tagsH True  = TagsInfoPkgs <$> withDbc queryAllTags -- FIXME: ETag

    tagsGetH :: TagName -> AppHandler (Set PkgN)
    tagsGetH tn = doEtagFoldableGet $ do
        r <- withDbc $ \dbconn -> do
            res <- PGS.query dbconn "SELECT pname FROM pname_tag WHERE tagname = ? ORDER BY pname" (PGS.Only tn)
            pure (Set.fromList $ map PGS.fromOnly res)

        when (r == mempty) $
            throwServantErr' err404

        pure r

    tagsPutH, tagsDelH :: TagName -> PkgN -> AppHandler NoContent
    tagsPutH tagn pkgn = tagSetH tagn pkgn >> pure NoContent
    tagsDelH tagn pkgn = tagDelH tagn pkgn >> pure NoContent

    ----------------------------------------------------------------------------
    -- queue v2 --------------------------------------------------------------

    queueGetH :: Handler App App [QEntryRow]
    queueGetH = doEtagFoldableGet $ withDbc $ \dbconn ->
      PGS.query_ dbconn "SELECT prio,modified,pname,ptime FROM queue ORDER BY prio desc, modified desc"

    queueGetPkgH :: PkgN -> Handler App App [QEntryRow]
    queueGetPkgH pname = doEtagFoldableGet $ withDbcGuard (pkgnExists pname) $ \dbconn ->
      PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue WHERE pname = ? ORDER BY prio desc, modified desc" (Only pname)

    -- FIXME: etag
    queueGetPkgIdxStH :: PkgN -> PkgIdxTs -> AppHandler QEntryRow
    queueGetPkgIdxStH pname idxstate = headOr404M $ withDbc $ \dbconn ->
      PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue \
                       \WHERE pname = ? AND ptime = ? \
                       \ORDER BY prio desc, modified desc"
                       (pname,idxstate)

    queuePutPkgIdxStH :: PkgN -> PkgIdxTs -> QEntryUpd -> AppHandler QEntryRow
    queuePutPkgIdxStH pname idxstate (QEntryUpd prio) = needAuth $ do
        headOr404M $ withDbcGuard check $ \dbconn -> do
            _ <- PGS.execute dbconn "INSERT INTO queue(pname,ptime,prio) \
                                    \VALUES (?,?,?) \
                                    \ON CONFLICT (pname,ptime) \
                                    \DO UPDATE SET prio = EXCLUDED.prio, modified = DEFAULT"
                                    (pname,idxstate,prio)

            PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue \
                             \WHERE pname = ? AND ptime = ? \
                             \ORDER BY prio desc, modified desc"
                             (pname,idxstate)
      where
        check dbc = liftM2 (&&) (pkgnExists pname dbc) (pkgIdxTsExists idxstate dbc)

    queueDelPkgIdxStH :: PkgN -> PkgIdxTs -> AppHandler NoContent
    queueDelPkgIdxStH pname idxstate = needAuth $ do
        cnt <- withDbc $ \dbconn -> PGS.execute dbconn "DELETE FROM queue WHERE pname = ? AND ptime = ?" (pname,idxstate)
        when (cnt == 0) $
            throwServantErr' err404
        pure NoContent

    ----------------------------------------------------------------------------
    -- queue -------------------------------------------------------------------

    queListH _cnt = do
        ents <- withDbc $ \dbconn -> do
            PGS.query_ dbconn "SELECT prio,modified,pname,ptime FROM queue ORDER BY prio desc, modified desc"
        pure $ mkListSlice 0 ents

    quePostH qent = needAuth $ withDbc $ \dbconn -> do
        let pn = qPackageName qent
            pp = qPriority qent

        _ <- PGS.execute dbconn "INSERT INTO queue(pname,prio) \
                                \VALUES (?,?) \
                                \ON CONFLICT (pname,ptime) \
                                \DO UPDATE SET prio = EXCLUDED.prio, modified = DEFAULT"
                                (pn,pp)
        pure ()

    queGetH pkgn = headOr404M $ withDbc $ \dbconn ->
        PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue WHERE pname = ?" (Only pkgn)

    quePutH pkgn prio = needAuth $ headOr404M $withDbc $ \dbconn ->
        PGS.query dbconn "UPDATE queue SET prio = ?, modified = DEFAULT \
                         \WHERE pname = ? \
                         \RETURNING prio,modified,pname,ptime"
                         (prio,pkgn)

    queDelH pkgn = needAuth $ do
        withDbc $ \dbconn -> do
            _ <- PGS.execute dbconn "DELETE FROM queue WHERE pname = ?" (PGS.Only pkgn)
            pure ()

    ----------------------------------------------------------------------------
    -- user -------------------------------------------------------------------

    usrListH :: UserName -> Handler App App UserPkgs
    usrListH uname = do
        mpkgs <- liftIO $ getUserInfoIO uname
        case mpkgs of
          Just pkgs -> pure (UserPkgs uname (uiPackages pkgs))
          Nothing   -> throwServantErr' err404

    usersListH :: UserName -> Handler App App UserPkgs
    usersListH uname = do
        mpkgs <- liftIO $ getUserInfoIO uname
        case mpkgs of
          Just pkgs -> pure (UserPkgs uname (uiPackages pkgs))
          Nothing   -> throwServantErr' err404


-- | Retrieve 'PkgLstCache' and update its content if stale
--
-- This operation is cheap on cache-hits, as it merely needs to
-- perform an O(1)-ish SQL lookup to test stale-ness.
fetchPkgLstCache :: AppHandler PkgLstCache
fetchPkgLstCache = do
    cacheMVar <- gets appPkgLstCache
    pool      <- gets appDbPool

    liftIO $ modifyMVar cacheMVar $ \cache0@(PkgLstCache is0 _) -> do
        withResource pool $ \dbconn -> do
            [Only is1] <- PGS.query_ dbconn "SELECT max(ptime) FROM idxstate"

            -- We assume that the 'pkgname' table is a function of 'max(ptime)'
            if is0 == is1
            then pure (cache0, cache0) -- no-op
            else do
               lst1 <- (coerce :: [Only PkgN] -> [PkgN]) <$> PGS.query_ dbconn "SELECT pname FROM pkgname ORDER by pname"
               lst1' <- evaluate (force (V.fromList lst1))
               let !cache1 = PkgLstCache is1 lst1'
               pure (cache1, cache1)

-- | Retrieve 'PkgIdxTsSet' cache and update its content if stale
fetchPkgIdxTsCache :: AppHandler PkgIdxTsSet
fetchPkgIdxTsCache = do
    cacheMVar <- gets appPkgIdxTsCache
    pool      <- gets appDbPool

    liftIO $ modifyMVar cacheMVar $ \pits -> do
        withResource pool $ \dbconn -> do
            let is0 = PkgIdxTsSet.findMax pits
            [Only is1] <- PGS.query_ dbconn "SELECT max(ptime) FROM idxstate"

            -- We assume that the 'idxstate' table grows monotonically/linearly
            if is0 == Just is1
            then pure (pits, pits) -- no-op
            else do
               lst1 <- (coerce :: [Only PkgIdxTs] -> [PkgIdxTs]) <$> PGS.query_ dbconn "SELECT ptime FROM idxstate ORDER by ptime"
               Just cache1 <- evaluate (force (PkgIdxTsSet.fromDistinctAscList lst1))
               pure (cache1, cache1)


withDbc :: (PGS.Connection -> IO a) -> AppHandler a
withDbc act = do
    pool <- gets appDbPool
    liftIO (withResource pool act)

withDbcGuard :: (PGS.Connection -> IO Bool) -> (PGS.Connection -> IO b) -> AppHandler b
withDbcGuard cond body = do
    res <- withDbc $ \dbconn -> do
        x <- cond dbconn
        if x
        then Just <$> body dbconn
        else pure Nothing

    maybe (throwServantErr' err404) pure res

headOr404M :: AppHandler [e] -> AppHandler e
headOr404M act = headOr404 =<< act
  where
    headOr404 :: [e] -> AppHandler e
    headOr404 []    = throwServantErr' err404
    headOr404 (e:_) = pure e

foldableToMaybe :: Foldable t => t a -> Maybe a
foldableToMaybe = foldr (\x _ -> Just x) Nothing

type AppHandler = Handler App App

queryAllTags :: PGS.Connection -> IO (Map TagName (Set PkgN))
queryAllTags dbconn = do
    res <- PGS.query_ dbconn "SELECT tagname,pname FROM pname_tag"
    pure $ Map.fromListWith (<>) [ (tn,Set.singleton pn) | (tn,pn) <- res ]

queryAllTagnames :: PGS.Connection -> IO (Set TagName)
queryAllTagnames dbconn = do
    tgs <- PGS.query_ dbconn "SELECT tagname FROM pname_tag ORDER by tagname"
    pure (Set.fromList $ map fromOnly tgs)

queryAllTagsInv :: PGS.Connection -> IO (Map PkgN (Set TagName))
queryAllTagsInv dbconn = do
    res <- PGS.query_ dbconn "SELECT tagname,pname FROM pname_tag"
    pure $ Map.fromListWith (<>) [ (pn,Set.singleton tn) | (tn,pn) <- res ]

is2lbs :: InputStream ByteString -> IO LBS.ByteString
is2lbs s = LBS.fromChunks <$> Streams.toList s


throwNotModified304 :: MonadSnap m => ByteString -> m b
throwNotModified304 etag = throwServantErr0 (err304 { errBody = "", errHeaders = [ (HTTP.hETag, etag) ]})

throwBasicAuth401 :: MonadSnap m => ByteString -> m b
throwBasicAuth401 realm =
    throwServantErr0 (err401 { errBody = "null", errHeaders = [ (HTTP.hContentType, "application/json")
                                                              , (HTTP.hWWWAuthenticate, "Basic realm=\"" <> realm <> "\"")
                                                              ] })

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
-- temporary implementation; long-term we need to keep the user-association replicated in our database

getUsersIO :: IO [UserNameId]
getUsersIO = do
    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })
    either (fail . show) pure =<< runExceptT (runClientM' manager hackageUrl $ getUsers)

getUserInfoIO :: UserName -> IO (Maybe UserInfo)
getUserInfoIO n = do
    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutNone })

    res <- runExceptT (runClientM' manager hackageUrl $ getUserInfo n)

    case res of
      Right ui -> pure (Just ui)

      Left (FailureResponse (Serv.Response
                             { responseStatusCode = HTTP.Status { statusCode = 404 }
                             , responseBody       = "User not found: Could not find user: not presently registered\n"
                             })) -> pure Nothing

      Left err -> fail (show err)

-- | Compute set of packages owned (i.e. being maintainer of) by user.
uiPackages :: UserInfo -> Set PkgN
uiPackages ui = Set.fromList [ pn | ["","package",pn0,"maintainers"] <- map (T.splitOn "/") (Set.toList $ uiGroups ui)
                                  , Just pn <- [pkgNFromText pn0]
                                  ]
