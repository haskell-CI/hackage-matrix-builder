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
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON (..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set             as Set
import           Data.String
import qualified Data.Text            as T
import           Data.Time
import           Rest
import qualified Rest.Resource        as R

import           Api.Root
import           Api.Types
import           Api.Utils
import           BuildTypes           hiding (PkgVerStatus (..))
import           Config

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
      liftIO (xcabalPackage pkgName) `orThrow` Busy

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [PackageMeta]
    handler r = do
      reports <- reportsByStamp
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
    handler r = listRange r <$> reportsByStamp

reportsByStamp :: MonadRoot m => m [ReportMeta]
reportsByStamp
   =  fmap (map toReportMeta . sortBy (flip $ comparing snd))
   .  liftIO . filesByStamp (".json" `isSuffixOf`)
  =<< liftRoot (asks $ reportDir . config)
  where
    toReportMeta :: (Text, UTCTime) -> ReportMeta
    toReportMeta (a,b) = ReportMeta
      { rmPackageName = PackageName . T.reverse . T.drop 5 . T.reverse $ a
      , rmModified    = b
      }

validatePackage :: MonadIO m => PackageName -> ExceptT Reason_ m ()
validatePackage pkgName = do
 s <- liftIO loadPackageNames
 maybe (throwError NotFound) (const $ return ()) . find (== pkgName) $ s

loadPackageNames :: IO (Set PackageName)
loadPackageNames = fromMaybe Set.empty . decode <$> L.readFile "packageNames.json"

loadPackageSummary :: IO (Maybe [PackageName])
loadPackageSummary = fmap (map summaryName) . decode <$> L.readFile "packages.json"

newtype PackageSummary = PackageSummary { summaryName :: PackageName }
  deriving (Eq, Show)

instance FromJSON PackageSummary where
  parseJSON = withObject "PackageMeta" $ fmap PackageSummary . (.: "packageName")
