{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Controller.Util
    ( serveFileMem
    , serveFolderMem
    , ServeObjSrc(..)
    , ServeObj(..)
    ) where

import           Prelude.Local                    as Prelude

import           Codec.Base64                     as B64
import qualified Codec.Compression.Brotli         as Brotli
import qualified Codec.Compression.GZip           as GZip
import qualified Crypto.Hash.MD5                  as MD5
import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BS.L
import qualified Data.Map.Strict                  as Map
import           Data.Time                        (Day (..), UTCTime (..))
import           Snap.Core
import           Snap.Internal.Parsing            (fullyParse)
import qualified System.Directory                 as Dir

import qualified Log

type ContentType = ByteString

data ServeObjSrc
  = ServeObjSrcMem BS.L.ByteString
  | ServeObjSrcFile FilePath !Word {- metadata TTL expressed in seconds; 0 means always re-check; use maxBound to never recheck  -}
  deriving (Generic)

instance NFData ServeObjSrc

data ServeObj src = ServeObj
    { sobjUrlpath     ::  ByteString -- ^ use "" for top-level entry; avoid leading & trailing slashes
    , sobjSlashRenorm :: !Bool -- ^ if 'True' an automatic HTTP redirect to the slash-normalised urlpath will be issued
    , sobjMimeType    ::  ContentType
    , sobjAllowComp   :: !Bool -- ^ 'False' forces @identity@ encoding; 'True' enables gzip+br compression
    , sobjSource      ::  src
    } deriving (Generic)

instance NFData src => NFData (ServeObj src)


{-# NOINLINE serveFolderMem #-}
-- | Construct in-mem compression-capable caching static file-server
serveFolderMem :: (MonadIO m, MonadSnap m1) => [ServeObj ServeObjSrc] -> m (m1 ())
serveFolderMem entries0 = do
    entries0' <- liftIO (mapM go entries0)

    let entries = Map.fromList [ (mkKey $ sobjUrlpath sobj, sobj) | sobj <- entries0' ]

    pure (serveFolderMem2 entries)
  where
    mkKey s0 | BS.null s0 = s0
             | otherwise  = BS.cons 0x2f s0

    go :: ServeObj ServeObjSrc -> IO (ServeObj ServeObjCached)
    go obj0 = case sobjSource obj0 of
                ServeObjSrcMem lbs ->
                  pure $ obj0 { sobjSource = ServeObjSrcMem' (mkCachedData (sobjAllowComp obj0) (sobjMimeType obj0) lbs) }
                ServeObjSrcFile fp ttl -> do
                  cd <- newMVar (dummyFMI, Nothing) -- will be populated on first GET
                  pure $ obj0 { sobjSource = ServeObjSrcFile' fp ttl cd }


{-# NOINLINE serveFileMem #-}
serveFileMem :: MonadSnap m => ByteString {- content-type -} -> BS.L.ByteString -> m ()
serveFileMem mime identityData = serveCachedData (mkCachedData True mime identityData)

----------------------------------------------------------------------------
-- internals

data FileMetaInfo = FMI { _fmiLastQuery :: UTCTime
                        , _fmiSizeMtime :: Maybe (Integer,UTCTime) -- 'Nothing' if missing/unreadable
                        } deriving Show

dummyFMI :: FileMetaInfo
dummyFMI = FMI (UTCTime (ModifiedJulianDay 0) 0) Nothing

getFileMetaInfo :: FilePath -> Word -> FileMetaInfo -> IO (FileMetaInfo,Bool {- is-changed -})
getFileMetaInfo fp ttl (FMI lq0 info0) = do
  lq <- getCurrentTime

  if (lq `diffUTCTime` lq0) < realToFrac ttl
    then
      pure (FMI lq0 info0, False)
    else do
      info' <- try $ (,) <$> Dir.getFileSize fp <*> Dir.getModificationTime fp
      let info = either (const Nothing) Just (info' :: Either SomeException (Integer,UTCTime))
      pure (FMI lq info, info0 /= info)

data ServeObjCached
  = ServeObjSrcMem' CachedData
  | ServeObjSrcFile' FilePath !Word (MVar (FileMetaInfo,Maybe CachedData))


serveFolderMem2 :: MonadSnap m => Map ByteString (ServeObj ServeObjCached) -> m ()
serveFolderMem2 entries = do
    rq <- getRequest

    let (upath0,_query)  = BS.span (/= 0x3f) (rqURI rq)
        (upath,slashes) = BS.spanEnd (== 0x2f) upath0
        hasSlashes     = not (BS.null slashes)
        -- hasSingleSlash = BS.length slashes == 1

        -- somewhat roundabout way of working around snap-core#285
        pinfo = case rqPathInfo rq of
          pinfo0 | BS.null pinfo0, not hasSlashes -> pinfo0 -- corner case
                 | otherwise                      -> BS.cons 0x2f pinfo0

        (pinfo',pinfo'') = BS.spanEnd (== 0x2f) pinfo

    case Map.lookup pinfo' entries of
      Nothing -> pass

      Just ServeObj{..}
        | sobjSlashRenorm, pinfo'' /= "/" -> redirect (upath `mappend` "/")
        | not sobjSlashRenorm, not (BS.null pinfo'') -> pass
        | otherwise -> serveObj ServeObj{..}

serveObj :: MonadSnap m => ServeObj ServeObjCached -> m ()
serveObj ServeObj{..} = case sobjSource of
  ServeObjSrcMem' cd -> serveCachedData cd
  ServeObjSrcFile' fp ttl mcd -> do
    cd <- liftIO $ modifyMVar mcd $ \(fmi0,cd0) -> do

      (fmi,changed) <- getFileMetaInfo fp ttl fmi0

      cd' <- case (fmi,changed) of
        (FMI _ Nothing,_) -> pure Nothing
        (_,False)         -> pure cd0
        (FMI _ (Just _),True) -> do
          raw <- BS.L.readFile fp
          evaluate (rnf raw)

          let cd'' = Just (mkCachedData sobjAllowComp sobjMimeType raw)

          -- avoid recompressing on spurious metadata changes
          if (cdQETag <$> cd'') == (cdQETag <$> cd0)
            then pure cd0
            else do
              Log.logInfo ("serveFolderMem: fs-backed entry " <> tshow fp <> " changed")
              pure cd''

      pure ((fmi,cd'),cd')

    case cd of
      Nothing -> do
        Log.logWarning ("serveFolderMem failed to serve " <> tshow fp)
        finishWith $ setResponseCode 404 emptyResponse
      Just cd' -> serveCachedData cd'



{-
serveFolderMem :: MonadSnap m => (ByteString,BS.L.ByteString) -> [(ByteString,ByteString,BS.L.ByteString)] -> m ()
serveFolderMem entry0 entries = do
  rq <- getRequest

  let upath = BS.takeWhile (/= 0x3f) (rqURI rq)
      hasSlash = BS.isSuffixOf "/" upath

  case rqPathInfo rq of
    "" | hasSlash -> serveCachedData cent0
       | otherwise -> redirect (upath `mappend` "/")

    ent | Just cd <- lookup ent cents -> serveCachedData cd

    _ -> finishWith $ setResponseCode 404 emptyResponse

  -- liftIO (print $ (rqPathInfo rq, rqURI rq))
  where
    -- this gets memoized
    cent0 = mkCachedData (fst entry0) (snd entry0)
    cents = [ (k, mkCachedData mi da) | (k, mi, da) <- entries ]
-}

data CachedData = CachedData
    { cdQETag       :: !ByteString -- quoted ETag
    , cdMime        :: !ContentType
    , cdIdentity    :: !ByteString
    , cdIdentityMd5 :: !ByteString
    , cdBr          ::  ByteString -- compressed lazily on demand; will be "" if not available
    , cdBrMd5       ::  ByteString
    , cdGzip        ::  ByteString -- compressed lazily on demand; will be "" if not available
    , cdGzipMd5     ::  ByteString
    }

mkCachedData :: Bool -> ByteString -> BS.L.ByteString -> CachedData
mkCachedData doComp cdMime dat = CachedData{..}
  where
    cdQETag       = mconcat [ "\"", B64.encode . BS.take 30 . SHA256.hash $ cdIdentity, "\"" ]
    cdIdentity    = BS.L.toStrict dat
    cdIdentityMd5 = B64.encode (MD5.hash cdIdentity)

    cdBr | doComp = BS.L.toStrict $ Brotli.compress $ BS.L.fromStrict cdIdentity
         | otherwise = mempty
    cdBrMd5 = if doComp then B64.encode (MD5.hash cdBr) else mempty

    cdGzip | doComp = BS.L.toStrict
                    $ GZip.compressWith GZip.defaultCompressParams { GZip.compressLevel = GZip.bestCompression }
                    $ BS.L.fromStrict cdIdentity
           | otherwise = mempty
    cdGzipMd5 = if doComp then B64.encode (MD5.hash cdGzip) else mempty

serveCachedData :: MonadSnap m => CachedData -> m ()
serveCachedData CachedData{..} = do
    rq <- getRequest

    let acceptedEncoding  = fromMaybe "" $ getHeader "Accept-Encoding" rq
        acceptedEncodings = either mempty id $ fullyParse acceptedEncoding acceptParser

    -- Quick'n'dirty etag/if-none-match logic
    -- NB: if-none-match header value is not properly parsed and may
    -- easily not be detected if it contains trailing whitespace etc
    let mNoneMatch = getHeader "If-None-Match" rq
    when (Just cdQETag == mNoneMatch) $
      finishWith $ setResponseCode 304 (setHeader "ETag" cdQETag emptyResponse)

    modifyResponse $ setHeader "ETag" cdQETag

    -- ok, at this point we know the last-modified time and the
    -- content-type. set those.
    modifyResponse $ setContentType cdMime
                   . setHeader "Cache-Control" "max-age=0, must-revalidate"
                   . setHeader "Vary" "Accept-Encoding"
                   . setResponseCode 200

    case () of
      _ | "br" `elem` acceptedEncodings, not (BS.null cdBr) -> do
            modifyResponse $ setContentLength (fromIntegral $ BS.length cdBr)
                           . setHeader "Content-Encoding" "br"
                           . setHeader "content-md5" cdBrMd5
            writeBS cdBr
        | "gzip" `elem` acceptedEncodings, not (BS.null cdGzip) -> do
            modifyResponse $ setContentLength (fromIntegral $ BS.length cdGzip)
                           . setHeader "Content-Encoding" "gzip"
                           . setHeader "content-md5" cdGzipMd5
            writeBS cdGzip
        | otherwise -> do -- fallback to identity encoding
            modifyResponse $ setContentLength (fromIntegral $ BS.length cdIdentity)
                           . setHeader "Content-Encoding" "identity"
                           . setHeader "content-md5" cdIdentityMd5
            writeBS cdIdentity

-- idea stolen from "Snap.Util.GZip"
--
-- simplified parser which ignores preference-values
acceptParser :: P.Parser [ByteString]
acceptParser = do
    xs <- ((:[]) <$> encoding) <|> pure []
    ys <- many (P.char ',' *> encoding)
    P.endOfInput
    pure $! force (xs ++ ys)
  where
    encoding = P.skipSpace *> c <* P.skipSpace

    c = do
        x <- coding
        qvalue <|> pure ()
        pure x

    -- consume q-value for the purpose of discarding it
    qvalue = do
        P.skipSpace
        void $! P.char ';'
        P.skipSpace
        void $! P.char 'q'
        P.skipSpace
        void $! P.char '='
        float
        pure ()

    coding = P.string "*" <|> P.takeWhile1 isCodingChar

    isCodingChar ch = P.isDigit ch || P.isAlpha_ascii ch || ch == '-' || ch == '_'

    float = do
        _ <- P.takeWhile P.isDigit
        (P.char '.' >> P.takeWhile P.isDigit >> pure ()) <|> pure ()
