{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
module Api.Utils
  ( listRange
  , unlimitedListRange
  , filesByStamp
  , getDirWithFilter
  , secure
  , tryReadFile
  , xcabalPackage
  , mkUnlimitedListing
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import           Data.List.Split         (splitOn)
import qualified Data.Map.Strict         as Map
import           Data.String
import           Data.String.Conversions
import           Data.String.ToString
import           Data.Text               (pack)
import           Data.Time
import           Happstack.Server.Auth   (basicAuth)
import           Happstack.Server.Monads
import           Path
import           Rest
import           Rest.Container          (List)
import           Rest.Dictionary         (Dict, FromMaybe, Param (Param))
import           Rest.Driver.Happstack   ()
import           Text.Read               (readMaybe)

import qualified Rest.Container          as C
import           Safe
import           System.IO
import           System.Process

import           Api.Types
import           Builder                 (parseXList, xcabalExe)
import           BuildTypes              hiding (PkgVerStatus (..))
import qualified BuildTypes              as BT
import           Paths


listRange :: Range -> [a] -> [a]
listRange r = take (count r) . drop (offset r)

unlimitedListRange :: Range -> [a] -> List a
unlimitedListRange r as = C.List
  { C.offset = offset r
  , C.count  = length items
  , C.items  = items
  }
  where
    items = take (count r) . drop (offset r) $ as

filesByStamp :: (FilePath -> Bool) -> Path a Dir -> IO [(Text, UTCTime)]
filesByStamp p dir
   =  fmap (map $ first cs)
   .  mapM (\fp -> fmap (fp,) . (getModificationTimeP =<<) . fmap (dir </>) . parseRelFile $ fp)
  <=< fmap (filter p)
   $  getDirectoryContentsP dir

getDirWithFilter :: (FilePath -> Bool) -> Path a Dir -> IO [Text]
getDirWithFilter p = fmap (map pack . filter p) . getDirectoryContentsP

secure :: Happstack m => ExceptT (Reason a) m ()
secure = do
  login <- liftIO . fmap (splitOn "/") . readFile $ "auth"
  case login of
    [u,p] ->
      lift $ basicAuth "127.0.0.1" (Map.fromList [(u, p)]) $ return ()
    _ -> do
      liftIO $ hPutStrLn stderr "Failure reading auth file"
      throwError Busy

xcabalPackage :: PackageName -> IO (Maybe Package)
xcabalPackage pkgName = do
  (ex,sout,serr) <- readProcessWithExitCode xcabalExe ["xlist", cs pkgName] ""
  case ex of
    ExitFailure e -> do
      hPutStrLn stderr $ "xcabal xlist failed with " ++ show e ++ ", stderr: " ++ serr
      return Nothing
    ExitSuccess -> return $ parseXcabal sout

parseXcabal :: String -> Maybe Package
parseXcabal s = do
  let elems = parseXList . map pack . lines $ s
  (pn,_,_,_,_) <- headMay elems
  return Package
    { pName     = fromString . toString $ pn
    , pVersions = map versions elems
    }
  where
    versions :: (PkgName, PkgVer, PkgRev, BT.PkgVerStatus, [PkgFlag]) -> VersionInfo
    versions = \case
      (_pkgName, pkgVer, pkgRev, pkgVerStatus, _pkgflags) -> VersionInfo
        { version    = VersionName . tshowPkgVer $ pkgVer
        , revision   = Revision pkgRev
        , preference = case pkgVerStatus of
           BT.NormalPref  -> Normal
           BT.UnPreferred -> UnPreferred
           BT.Deprecated  -> Deprecated
        }

-- | A version of 'Rest.Handler.mkListing' with no upper limit for the
-- @count@ parameter.
--
-- Be careful not to use this for listings that are expensive to
-- compute (and if you use mkListing, enforce a maximum yourself).
mkUnlimitedListing :: Monad m
  => (Dict () () 'Nothing 'Nothing 'Nothing
  -> Dict h x i' o' e')
  -> (Range -> ExceptT (Reason (FromMaybe Void e')) m (FromMaybe () o'))
  -> Handler m
mkUnlimitedListing d a = mkHandler (mkPar unlimitedRange . d) (a . param)
  where
    unlimitedRange :: Param Range
    unlimitedRange = Param ["offset", "count"] $ \xs ->
      maybe (Left (ParseError "range"))
            (Right . normalize)
        $ case xs of
            [Just o, Just c] -> Range         <$> readMaybe o <*> readMaybe c
            [_     , Just c] -> Range 0       <$> readMaybe c
            [Just o, _     ] -> (`Range` 100) <$> readMaybe o
            _                -> Just $ Range 0 100
      where
        normalize r = Range
          { offset = max 0 . offset $ r
          , count  = max 0 . count $ r
          }
