{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -fno-warn-orphans #-}
module Api.Package
  ( resource
--  , reportsByStamp
  ) where

import           Control.Monad.Except
import           Data.Aeson           (FromJSON (..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map.Strict      as Map
import           Data.Ord
import           Data.String
import qualified Data.Text            as T
import           Data.Time
import           Rest
import qualified Rest.Resource        as R

import           Api.Root             (Root)
import           Api.Types
import           Api.Utils
import           BuildTypes

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
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithPackage ()
    handler = return ()

newtype PackageMeta = PackageMeta { meta_name :: PackageName }
  deriving (Eq, Show)

instance FromJSON PackageMeta where
  parseJSON = withObject "PackageMeta" $ fmap PackageMeta . (.: "packageName")

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [PackageListItem]
    handler r = do
      reports <- liftIO reportsByStamp
      pkgs <- liftIO (decode <$> L.readFile "packages.json") `orThrow` Busy
      return . listRange r . f reports . map meta_name $ pkgs
    f :: [ReportTime] -> [PackageName] -> [PackageListItem]
    f reps pkgs
      = map (\(pn,rs) -> PackageListItem { pliName = pn, pliReportStamp = rs })
      . Map.toList
      . foldl' (\pm rt -> Map.insert (rt_packageName rt) (Just $ rt_reportStamp rt) pm) pkgMap
      $ reps
      where
        pkgMap :: Map PackageName (Maybe a)
        pkgMap = Map.fromList . map (,Nothing) $ pkgs

listLatestReport :: ListHandler Root
listLatestReport = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [ReportTime]
    handler r = listRange r <$> liftIO reportsByStamp

reportsByStamp :: IO [ReportTime]
reportsByStamp
   =  fmap (map toReportTime . sortBy (flip $ comparing snd)) $ filesByStamp (".json" `isSuffixOf`) "report"
  where
    toReportTime :: (Text, UTCTime) -> ReportTime
    toReportTime (a,b) = ReportTime
      { rt_packageName = PackageName . T.reverse . T.drop 5 . T.reverse $ a
      , rt_reportStamp = b
      }
