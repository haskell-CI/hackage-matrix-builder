{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Api.Types where

import           Control.Arrow
import           Control.Monad.Reader   (ReaderT)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.JSON.Schema
import           Data.List              (groupBy)
import qualified Data.Map.Strict        as Map
import           Data.String
import           Data.String.ToString
import           Data.Time
import           Generics.Generic.Aeson
import           Rest.Info
import           Rest.ShowUrl

import           Api.Root               (Root)
import           BuildReport
import           BuildTypes

newtype PackageName = PackageName { unPackageName :: Text }
 deriving (FromJSON, Eq, IsString, JSONSchema, Ord, Show, ToJSON, ToString, ShowUrl)
instance Info PackageName where
  describe _ = "identifier"

type WithPackage = ReaderT PackageName Root

newtype Version = Version { unVersion :: Text }
  deriving (Eq, FromJSON, JSONSchema, Ord, Show, ToJSON)

data PackageListItem = PackageListItem
  { pliName        :: PackageName
  , pliReportStamp :: Maybe UTCTime
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON     PackageListItem where toJSON    = gtoJsonWithSettings    packageListItemSettings
instance FromJSON   PackageListItem where parseJSON = gparseJsonWithSettings packageListItemSettings
instance JSONSchema PackageListItem where schema    = gSchemaWithSettings    packageListItemSettings
packageListItemSettings :: Settings
packageListItemSettings = Settings { stripPrefix = Just "pli" }

data ReportTime = ReportTime
  { rt_packageName :: PackageName
  , rt_reportStamp :: UTCTime
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON     ReportTime where toJSON    = gtoJsonWithSettings    reportTimeSettings
instance FromJSON   ReportTime where parseJSON = gparseJsonWithSettings reportTimeSettings
instance JSONSchema ReportTime where schema    = gSchemaWithSettings    reportTimeSettings
reportTimeSettings :: Settings
reportTimeSettings = Settings { stripPrefix = Just "rt_" }

data ReportDataJson = ReportDataJson
  { rdj_packageName :: PackageName
  , rdj_versions    :: [[[Ver]]]
  , rdj_ghcVersions :: [GVer]
  } deriving (Generic, Show)
instance ToJSON     ReportDataJson where toJSON    = gtoJsonWithSettings reportDataJsonSettings
instance FromJSON   ReportDataJson where parseJSON = gparseJsonWithSettings reportDataJsonSettings
instance JSONSchema ReportDataJson where schema    = gSchemaWithSettings reportDataJsonSettings
reportDataJsonSettings :: Settings
reportDataJsonSettings = Settings { stripPrefix = Just "rdj_" }

reportDataJson :: ReportData -> ReportDataJson
reportDataJson = \case
  ReportData
    { rdPkgName   = a
    , rdVersions  = b
    , rdGVersions = c
    } -> ReportDataJson
      { rdj_packageName = fromString . toString $ a
      , rdj_versions    = map (map (map toVer)) . map (groupBy minorVersionGrouping) . groupBy majorVersionGrouping . Map.toList $ b
      , rdj_ghcVersions = map f . Map.toList $ c
      }
    where
      toVer (x,(y,z)) = Ver
        { version       = Version $ tshowPkgVer x
        , revision      = y
        , bo            = z
        }
      f :: (GhcVer, (PkgVer, Map PkgVer BuildResult, Map PkgVerPfx (Maybe PkgVer)))
        -> GVer
      f (w,(x,y,z)) = GVer
        { ghcVer     = ghcVerToV w
        , packageVer = pkgVerToV x
        , resultsA   = map toVersionResult . Map.toList $ y
        , resultsB   = map (second $ fmap pkgVerToV) . Map.toList $ z
        }
      toVersionResult (v,r) = VersionResult { packageVersion = pkgVerToV v, result = br r }
      br :: BuildResult -> Result
      br = \case
        BuildOk         -> Ok
        BuildNop        -> Nop
        BuildNoIp       -> NoIp
        BuildFail t     -> Fail t
        BuildFailDeps l -> FailDeps . map (\((xx,xy),y) -> DepFailure
          { dfPackageName    = fromString $ toString xx
          , dfPackageVersion = pkgVerToV xy
          , dfMessage        = y
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

pkgVerToV :: PkgVer -> Version
pkgVerToV = Version . tshowPkgVer

ghcVerToV :: GhcVer -> Version
ghcVerToV = Version . ghcVerName

data Ver = Ver
  { version  :: Version
  , revision :: PkgRev
  , bo       :: Bool
  } deriving (Generic, Show)

instance ToJSON     Ver where toJSON    = gtoJson
instance FromJSON   Ver where parseJSON = gparseJson
instance JSONSchema Ver where schema    = gSchema

data GVer = GVer
  { ghcVer     :: Version
  , packageVer :: Version
  , resultsA   :: [VersionResult]
  , resultsB   :: [(PkgVerPfx, Maybe Version)]
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

ghcVerName :: GhcVer -> Text
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
  { packageVersion :: Version
  , result         :: Result
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

data DepFailure = DepFailure
  { dfPackageName    :: PackageName
  , dfPackageVersion :: Version
  , dfMessage        :: Text
  } deriving (Generic, Show)
instance ToJSON     DepFailure where toJSON    = gtoJsonWithSettings depFailureSettings
instance FromJSON   DepFailure where parseJSON = gparseJsonWithSettings depFailureSettings
instance JSONSchema DepFailure where schema    = gSchemaWithSettings depFailureSettings
depFailureSettings :: Settings
depFailureSettings = Settings $ Just "df"
