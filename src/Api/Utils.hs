{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
module Api.Utils
  ( listRange
  , filesByStamp
  , getDirWithFilter
  , secure
  , tryReadFile
  , xcabalPackage
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
