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

import           Control.Monad.State
import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map.Strict                  as Map
import           Data.Pool
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as PGS
import           Database.PostgreSQL.Simple.Types (Only (..))
import qualified Network.HTTP.Types.Header        as HTTP
import           Servant
import           Snap.Core
import           Snap.Http.Server                 (defaultConfig)
import qualified Snap.Http.Server.Config          as Snap
import           Snap.Snaplet
import qualified Snap.Util.FileServe              as Snap
import qualified System.IO.Streams                as Streams

-- local modules
import           Controller.Api
import           Controller.Db
import           PkgId

data App = App
  { appDbPool   :: Pool PGS.Connection
  }

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

mkListSlice :: Word -> [a] -> ListSlice a
mkListSlice ofs xs = ListSlice ofs (fromIntegral $ length xs) xs

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

    needAuth :: AppHandler a -> AppHandler a
    needAuth h = do
        rq <- getRequest
        let mauth = getHeader "Authorization" rq

        case fmap BS.words mauth of
          Just ["Basic",cred] -- FIXME; quick hack
            | s3cr3t == SHA256.hash cred -> pure ()
            | otherwise -> do
                  liftIO $ print ("bad credentials: " <> cred)
                  throwBasicAuth401 "Hackage Matrix"
          _ -> throwBasicAuth401 "Hackage Matrix"
        h
      where
        s3cr3t = "w/\207\193f\ENQ\248\DLE\202\160.\181\203<\188!\161U\147\232\DC4R\151\152g\188\218d\NAK\SI\150\172"

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
            let ents = [ PkgListEntry pn (Map.findWithDefault [] pn tags) (fmap ptime2utc pt)
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

    quePostH qent = needAuth $ withDbc $ \dbconn -> do
        let pn = qPackageName qent
            pp = qPriority qent

        _ <- PGS.execute dbconn "INSERT INTO queue(pname,prio) \
                                \VALUES (?,?) \
                                \ON CONFLICT (pname,ptime) \
                                \DO UPDATE SET prio = EXCLUDED.prio, modified = DEFAULT"
                                (pn,pp)
        pure ()

    queGetH pkgn = do
        ents <- withDbc $ \dbconn -> do
            PGS.query dbconn "SELECT prio,modified,pname,ptime FROM queue WHERE pname = ?" (PGS.Only pkgn)
        case ents of
          []    -> throwServantErr' err404
          (e:_) -> pure e

    quePutH pkgn prio = needAuth $ do
        ents <- withDbc $ \dbconn -> do
            PGS.query dbconn "UPDATE queue SET prio = ?, modified = DEFAULT \
                             \WHERE pname = ? \
                             \RETURNING prio,modified,pname,ptime"
                             (prio,pkgn)

        case ents of
          []    -> throwServantErr' err404
          (e:_) -> pure e

    queDelH pkgn = needAuth $ do
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
