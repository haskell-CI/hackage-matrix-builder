{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -fno-warn-orphans #-}
module Api.Package (resource, WithPackage, Identifier (..)) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON (..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as L
import           Data.List            (foldl', isSuffixOf)
import qualified Data.Map.Strict      as Map
import           Data.Ord
import           Data.Text            (pack, unpack)
import qualified Data.Text            as T
import           Data.Time
import           Rest
import           Rest.Info            (Info (..))
import qualified Rest.Resource        as R
import           Rest.ShowUrl
import           Safe
import           System.Directory

import           Api.Root             (Root)
import           Api.Types
import           Api.Utils
import           BuildTypes

data Identifier
  = Name Text
  | LatestReport

data Listing
  = All
  | LatestReports

instance Info Identifier where
  describe _ = "identifier"

instance ShowUrl Identifier where
  showUrl = \case
    Name t       -> unpack t
    LatestReport -> "latest-report"

type WithPackage = ReaderT Identifier Root

resource :: Resource Root WithPackage Identifier Listing Void
resource = mkResourceReader
  { R.name   = "package"
  , R.schema = withListing All $ named [ ("name"          , singleBy (Name . pack))
                                       , ("latest-report" , single LatestReport)
                                       , ("latest-reports", listing LatestReports)
                                       ]
  , R.get    = Just get
  , R.list   = \case
     All           -> list
     LatestReports -> listLatestReport
  }

newtype PackageMeta = PackageMeta { meta_name :: Text }
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
    f :: [ReportTime] -> [Text] -> [PackageListItem]
    f reps pkgs
      = map (\(pn,rs) -> PackageListItem { pliName = pn, pliReportStamp = rs })
      . Map.toList
      . foldl' (\pm rt -> Map.insert (rt_packageName rt) (Just $ rt_reportStamp rt) pm) pkgMap
      $ reps
      where
        pkgMap :: Map Text (Maybe a)
        pkgMap = Map.fromList . map (,Nothing) $ pkgs

listLatestReport :: ListHandler Root
listLatestReport = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [ReportTime]
    handler r = listRange r <$> liftIO reportsByStamp

get :: Handler WithPackage
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithPackage ReportDataJson
    handler = do
      ident <- ask
      case ident of
        Name t      -> byName t
        LatestReport -> do
          latest <- liftIO (fmap headMay reportsByStamp) `orThrow` NotFound
          byName $ rt_packageName latest
    byName :: Text -> ExceptT Reason_ WithPackage ReportDataJson
    byName t = do
      let fp = "report/" ++ unpack t ++ ".json"
      exists <- liftIO $ doesFileExist fp
      unless exists $ throwError NotFound
      f <- liftIO $ L.readFile fp
      maybe (throwError Busy) (return . reportDataJson) . decode $ f

reportsByStamp :: IO [ReportTime]
reportsByStamp
   =  fmap (map toReportTime) $ filesByStamp (flip $ comparing snd) (".json" `isSuffixOf`) "report"
  where
    toReportTime :: (Text,UTCTime) -> ReportTime
    toReportTime (a,b) = ReportTime
      { rt_packageName = T.reverse . T.drop 5 . T.reverse $ a
      , rt_reportStamp = b
      }
