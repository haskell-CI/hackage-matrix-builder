{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.Package
  ( resource
  , validatePackage
  , loadPackageSummary
  ) where

import           Control.Monad.Except
import           Data.Aeson           (FromJSON (..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map.Strict      as Map
import           Data.Ord
import           Data.String
import           Data.String.ToString
import           Data.Text            (pack)
import qualified Data.Text            as T
import           Data.Time
import           Rest
import qualified Rest.Resource        as R
import           Safe
import           System.IO
import           System.Process

import           Api.Root             (Root)
import           Api.Types
import           Api.Utils
import           Builder              (parseXList, xcabalExe)
import           BuildTypes           hiding (PkgVerStatus (..))
import qualified BuildTypes           as BT

data Listing
  = All
  | LatestReports

resource :: Resource Root WithPackage PackageName Listing Void
resource = mkResourceReader
  { R.name   = "package"
  , R.schema = withListing All $ named [ ("name"          , singleBy fromString)
                                       , ("latest-reports", listing LatestReports)
                                       ]
  , R.get    = Just get
  , R.list   = \case
     All           -> list
     LatestReports -> listLatestReport
  }

get :: Handler WithPackage
get = mkIdHandler jsonO $ const handler
  where
    handler :: PackageName -> ExceptT Reason_ WithPackage Package
    handler pkgName = do
      validatePackage pkgName
      (ex,sout,serr) <- liftIO $ readProcessWithExitCode xcabalExe ["xlist", toString pkgName] ""
      case ex of
        ExitFailure e -> do
          liftIO $ hPutStrLn stderr $ "xcabal xlist failed with " ++ show e ++ ", stderr: " ++ serr
          throwError Busy
        ExitSuccess -> maybe (throwError Busy) return $ parseXcabal sout

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

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [PackageMeta]
    handler r = do
      reports <- liftIO reportsByStamp
      pkgs <- liftIO loadPackageSummary `orThrow` Busy
      return . listRange r . f reports $ pkgs
    f :: [ReportMeta] -> [PackageName] -> [PackageMeta]
    f reps pkgs
      = map (\(pn,rs) -> PackageMeta { pmName = pn, pmReport = rs })
      . Map.toList
      . foldl' (\pm rt -> Map.insert (rmPackageName rt) (Just $ rmModified rt) pm) pkgMap
      $ reps
      where
        pkgMap :: Map PackageName (Maybe a)
        pkgMap = Map.fromList . map (,Nothing) $ pkgs

listLatestReport :: ListHandler Root
listLatestReport = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [ReportMeta]
    handler r = listRange r <$> liftIO reportsByStamp

reportsByStamp :: IO [ReportMeta]
reportsByStamp
   =  fmap (map toReportMeta . sortBy (flip $ comparing snd)) $ filesByStamp (".json" `isSuffixOf`) "report"
  where
    toReportMeta :: (Text, UTCTime) -> ReportMeta
    toReportMeta (a,b) = ReportMeta
      { rmPackageName = PackageName . T.reverse . T.drop 5 . T.reverse $ a
      , rmModified    = b
      }

validatePackage :: MonadIO m => PackageName -> ExceptT Reason_ m ()
validatePackage pkgName = do
 s <- liftIO loadPackageSummary `orThrow` NotFound
 maybe (throwError NotFound) (const $ return ()) . find (== pkgName) $ s

loadPackageSummary :: IO (Maybe [PackageName])
loadPackageSummary = fmap (map summaryName) . decode <$> L.readFile "packages.json"

newtype PackageSummary = PackageSummary { summaryName :: PackageName }
  deriving (Eq, Show)

instance FromJSON PackageSummary where
  parseJSON = withObject "PackageMeta" $ fmap PackageSummary . (.: "packageName")
