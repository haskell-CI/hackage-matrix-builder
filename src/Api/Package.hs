{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS -fno-warn-orphans #-}
module Api.Package (resource, WithPackage, Identifier (..)) where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson             (FromJSON (..), ToJSON (..), decode,
                                         withObject, (.:))
import qualified Data.ByteString.Lazy   as L
import           Data.JSON.Schema
import           Data.List              (groupBy, isSuffixOf, sort, sortBy)
import qualified Data.Map.Strict        as Map
import           Data.Ord
import           Data.String.ToString
import           Data.Text              (pack, unpack)
import qualified Data.Text              as T
import           Data.Time
import           Generics.Generic.Aeson
import           Rest
import           Rest.Info              (Info (..))
import qualified Rest.Resource          as R
import           Rest.ShowUrl
import           Safe
import           System.Directory

import           Api.Types
import           BuildReport
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
    handler :: Range -> ExceptT Reason_ Root [Text]
    handler r = do
      pkgs <- liftIO (decode <$> L.readFile "packages.json") `orThrow` Busy
      return . listRange r . sort . map meta_name $ pkgs

data ReportTime = ReportTime
  { rt_packageName :: Text
  , rt_reportStamp :: UTCTime
  } deriving (Eq, Generic, Show)

instance ToJSON     ReportTime where toJSON    = gtoJsonWithSettings    reportTimeSettings
instance FromJSON   ReportTime where parseJSON = gparseJsonWithSettings reportTimeSettings
instance JSONSchema ReportTime where schema    = gSchemaWithSettings    reportTimeSettings

reportTimeSettings :: Settings
reportTimeSettings = Settings { stripPrefix = Just "rt_" }

listLatestReport :: ListHandler Root
listLatestReport = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [ReportTime]
    handler r = listRange r <$> liftIO reportsByStamp

listRange :: Range -> [a] -> [a]
listRange r = take (count r) . drop (offset r)

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
   =  fmap (map toReportTime)
   .  fmap (sortBy (flip $ comparing snd))
   .  mapM (\fp -> (fp,) <$> getModificationTime ("report/" ++ fp))
  <=< fmap (filter (".json" `isSuffixOf`))
   $  getDirectoryContents "report/"
  where
    toReportTime :: (String,UTCTime) -> ReportTime
    toReportTime (a,b) = ReportTime
      { rt_packageName = pack . reverse . drop 5 . reverse $ a
      , rt_reportStamp = b
      }

data ReportDataJson = ReportDataJson
  { pkgName     :: String
  , versions    :: [[[Ver]]]
  , ghcVersions :: [GVer]
  } deriving (Generic, Show)

reportDataJson :: ReportData -> ReportDataJson
reportDataJson = \case
  ReportData
    { rdPkgName   = a
    , rdVersions  = b
    , rdGVersions = c
    } -> ReportDataJson
      { pkgName     = toString a
      , versions    = map (map (map toVer)) . map (groupBy minorVersionGrouping) . groupBy majorVersionGrouping . Map.toList $ b
      , ghcVersions = map f . Map.toList $ c
      }
    where
      toVer (x,(y,z)) = Ver
        { version       = V { segments = unPkgVer x, name = T.unpack $ tshowPkgVer x }
        , revision      = y
        , bo            = z
        }
      f :: (GhcVer, (PkgVer, Map PkgVer BuildResult, Map PkgVerPfx (Maybe PkgVer)))
        -> GVer
      f (w,(x,y,z)) = GVer
        { ghcVer   = ghcVerToV w
        , pkgVer   = pkgVerToV x
        , resultsA = map toVersionResult . Map.toList $ y
        , resultsB = map (second $ fmap pkgVerToV) . Map.toList $ z
        }
      toVersionResult (v,r) = VersionResult { pkgVersion = pkgVerToV v, result = br r }
      br :: BuildResult -> Result
      br = \case
        BuildOk         -> Ok
        BuildNop        -> Nop
        BuildNoIp       -> NoIp
        BuildFail t     -> Fail t
        BuildFailDeps l -> FailDeps . map (\((xx,xy),y) -> DepFailure
          { pkgId   = PackageId
            { pPackageName    = toString xx
            , pPackageVersion = pkgVerToV xy
          }
          , message = y
          }) $ l

majorVersionGrouping :: (PkgVer, a) -> (PkgVer, a) -> Bool
majorVersionGrouping (a,_) (b,_) = verMajor a == verMajor b

minorVersionGrouping :: (PkgVer, a) -> (PkgVer, a) -> Bool
minorVersionGrouping (a,_) (b,_) = verMinor a == verMinor b

verMajor :: PkgVer -> PkgVerPfx
verMajor v = case unPkgVer v of
  []    -> []
  [a]   -> [a,0]
  a:b:_ -> [a,b]

verMinor :: PkgVer -> PkgVerPfx
verMinor v = case unPkgVer v of
  []      -> []
  [a]     -> [a,0,0]
  [a,b]   -> [a,b,0]
  a:b:c:_ -> [a,b,c]

pkgVerToV :: PkgVer -> V
pkgVerToV p = V (unPkgVer p) (T.unpack . tshowPkgVer $ p)

ghcVerToV :: GhcVer -> V
ghcVerToV g = V
  { segments = ghcVerSegments g
  , name     = ghcVerName     g
  }

instance ToJSON     ReportDataJson where toJSON    = gtoJson
instance FromJSON   ReportDataJson where parseJSON = gparseJson
instance JSONSchema ReportDataJson where schema    = gSchema

data V = V
  { segments :: [Word]
  , name     :: String
  } deriving (Show, Generic)

instance ToJSON     V where toJSON    = gtoJson
instance FromJSON   V where parseJSON = gparseJson
instance JSONSchema V where schema    = gSchema

data Ver = Ver
  { version  :: V
  , revision :: PkgRev
  , bo       :: Bool
  } deriving (Generic, Show)

instance ToJSON     Ver where toJSON    = gtoJson
instance FromJSON   Ver where parseJSON = gparseJson
instance JSONSchema Ver where schema    = gSchema

data GVer = GVer
  { ghcVer   :: V
  , pkgVer   :: V
  , resultsA :: [VersionResult]
  , resultsB :: [(PkgVerPfx, Maybe V)]
  } deriving (Generic, Show)

instance ToJSON     GVer where toJSON    = gtoJson
instance FromJSON   GVer where parseJSON = gparseJson
instance JSONSchema GVer where schema    = gSchema

ghcVerSegments :: GhcVer -> [Word]
ghcVerSegments = \case
  GHC_7_00 -> [7,0]
  GHC_7_02 -> [7,2]
  GHC_7_04 -> [7,4]
  GHC_7_06 -> [7,6]
  GHC_7_08 -> [7,8]
  GHC_7_10 -> [7,10]

ghcVerName :: GhcVer -> String
ghcVerName = \case
  GHC_7_00 -> "7.0"
  GHC_7_02 -> "7.2"
  GHC_7_04 -> "7.4"
  GHC_7_06 -> "7.6"
  GHC_7_08 -> "7.8"
  GHC_7_10 -> "7.10"

unPkgVer :: PkgVer -> [Word]
unPkgVer (PkgVer ws) = ws

data VersionResult = VersionResult
  { pkgVersion :: V
  , result     :: Result
  } deriving (Generic, Show)
instance ToJSON     VersionResult where toJSON    = gtoJson
instance FromJSON   VersionResult where parseJSON = gparseJson
instance JSONSchema VersionResult where schema    = gSchema

data Result
  = Ok
  | Nop
  | NoIp
  | Fail Text
  | FailDeps [DepFailure]
  deriving (Generic, Show)
instance ToJSON     Result where toJSON    = gtoJson
instance FromJSON   Result where parseJSON = gparseJson
instance JSONSchema Result where schema    = gSchema

data PackageId = PackageId
  { pPackageName    :: String
  , pPackageVersion :: V
  } deriving (Generic, Show)
instance ToJSON     PackageId where toJSON    = gtoJson
instance FromJSON   PackageId where parseJSON = gparseJson
instance JSONSchema PackageId where schema    = gSchema

data DepFailure = DepFailure
  { pkgId   :: PackageId
  , message :: Text
  } deriving (Generic, Show)
instance ToJSON     DepFailure where toJSON    = gtoJson
instance FromJSON   DepFailure where parseJSON = gparseJson
instance JSONSchema DepFailure where schema    = gSchema
