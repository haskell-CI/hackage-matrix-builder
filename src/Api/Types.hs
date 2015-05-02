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
import           Api.Utils
import           BuildReport
import           BuildTypes

newtype PackageName = PackageName { unPackageName :: Text }
 deriving (FromJSON, Eq, IsString, JSONSchema, Ord, Show, ToJSON, ToString, ShowUrl)
instance Info PackageName where
  describe _ = "identifier"

type WithPackage = ReaderT PackageName Root

newtype VersionName = VersionName { unVersionName :: Text }
  deriving (Eq, FromJSON, JSONSchema, Ord, Show, ToJSON)

newtype Revision = Revision { unRevision :: Word }
  deriving (Eq, FromJSON, JSONSchema, Ord, Show, ToJSON)

data PackageMeta = PackageMeta
  { pmName   :: PackageName
  , pmReport :: Maybe UTCTime
  } deriving (Eq, Generic, Ord, Show)
instance ToJSON     PackageMeta where toJSON    = gtoJsonWithSettings    $ strip "pm"
instance FromJSON   PackageMeta where parseJSON = gparseJsonWithSettings $ strip "pm"
instance JSONSchema PackageMeta where schema    = gSchemaWithSettings    $ strip "pm"

data ReportMeta = ReportMeta
  { rmPackageName :: PackageName
  , rmModified    :: UTCTime
  } deriving (Eq, Generic, Ord, Show)
instance ToJSON     ReportMeta where toJSON    = gtoJsonWithSettings    $ strip "rm"
instance FromJSON   ReportMeta where parseJSON = gparseJsonWithSettings $ strip "rm"
instance JSONSchema ReportMeta where schema    = gSchemaWithSettings    $ strip "rm"

data Report = Report
  { rPackageName :: PackageName
  , rVersions    :: [[[VersionInfo]]]
  , rResults     :: [GHCResult]
  } deriving (Eq, Generic, Show)
instance ToJSON     Report where toJSON    = gtoJsonWithSettings    $ strip "r"
instance FromJSON   Report where parseJSON = gparseJsonWithSettings $ strip "r"
instance JSONSchema Report where schema    = gSchemaWithSettings    $ strip "r"

toReport :: ReportData -> Report
toReport rd = Report
  { rPackageName = fromString . toString . rdPkgName $ rd
  , rVersions    = map (map (map toVersionInfo)) . map (groupBy minorVersionGrouping) . groupBy majorVersionGrouping . Map.toList . rdVersions $ rd
  , rResults = map f . Map.toList . rdGVersions $ rd
  }
  where
    toVersionInfo (x,(y,z)) = VersionInfo
      { version       = VersionName $ tshowPkgVer x
      , revision      = Revision y
      , unpreferred   = z
      }
    f :: (GhcVer, (PkgVer, Map PkgVer BuildResult, Map PkgVerPfx (Maybe PkgVer)))
      -> GHCResult
    f (w,(x,y,z)) = GHCResult
      { ghcVersion     = ghcVersionName w
      , ghcFullVersion = VersionName $ tshowPkgVer x
      , resultsA       = map toVersionResult . Map.toList $ y
      , resultsB       = map (second $ fmap (VersionName . tshowPkgVer)) . Map.toList $ z
      }
    toVersionResult (v,r) = VersionResult
      { packageVersion = VersionName $ tshowPkgVer v
      , result         = br r
      }
    br :: BuildResult -> Result
    br = \case
      BuildOk         -> Ok
      BuildNop        -> Nop
      BuildNoIp       -> NoIp
      BuildFail t     -> Fail t
      BuildFailDeps l -> FailDeps . map (\((xx,xy),y) -> DepFailure
        { dfPackageName    = fromString $ toString xx
        , dfPackageVersion = VersionName $ tshowPkgVer xy
        , dfMessage        = y
        }) $ l

majorVersionGrouping :: (PkgVer, a) -> (PkgVer, a) -> Bool
majorVersionGrouping (a,_) (b,_) = verMajor a == verMajor b
  where
    verMajor :: PkgVer -> PkgVerPfx
    verMajor v = case unPkgVer v of
      []    -> []
      [x]   -> [x,0]
      x:y:_ -> [x,y]
    unPkgVer :: PkgVer -> [Word]
    unPkgVer (PkgVer ws) = ws

minorVersionGrouping :: (PkgVer, a) -> (PkgVer, a) -> Bool
minorVersionGrouping (a,_) (b,_) = verMinor a == verMinor b
  where
    verMinor :: PkgVer -> PkgVerPfx
    verMinor v = case unPkgVer v of
      []      -> []
      [x]     -> [x,0,0]
      [x,y]   -> [x,y,0]
      x:y:z:_ -> [x,y,z]
    unPkgVer :: PkgVer -> [Word]
    unPkgVer (PkgVer ws) = ws

data VersionInfo = VersionInfo
  { version     :: VersionName
  , revision    :: Revision
  , unpreferred :: Bool
  } deriving (Eq, Generic, Show)
instance ToJSON     VersionInfo where toJSON    = gtoJson
instance FromJSON   VersionInfo where parseJSON = gparseJson
instance JSONSchema VersionInfo where schema    = gSchema

data GHCResult = GHCResult
  { ghcVersion     :: VersionName
  , ghcFullVersion :: VersionName
  , resultsA       :: [VersionResult]
  , resultsB       :: [(PkgVerPfx, Maybe VersionName)]
  } deriving (Eq, Generic, Show)
instance ToJSON     GHCResult where toJSON    = gtoJson
instance FromJSON   GHCResult where parseJSON = gparseJson
instance JSONSchema GHCResult where schema    = gSchema

ghcVersionName :: GhcVer -> VersionName
ghcVersionName = VersionName . \case
  GHC_7_00 -> "7.0"
  GHC_7_02 -> "7.2"
  GHC_7_04 -> "7.4"
  GHC_7_06 -> "7.6"
  GHC_7_08 -> "7.8"
  GHC_7_10 -> "7.10"

data VersionResult = VersionResult
  { packageVersion :: VersionName
  , result         :: Result
  } deriving (Eq, Generic, Show)
instance ToJSON     VersionResult where toJSON    = gtoJson
instance FromJSON   VersionResult where parseJSON = gparseJson
instance JSONSchema VersionResult where schema    = gSchema

data Result
  = Ok
  | Nop
  | NoIp
  | Fail Text
  | FailDeps [DepFailure]
  deriving (Eq, Generic, Show)
instance ToJSON     Result where toJSON    = gtoJson
instance FromJSON   Result where parseJSON = gparseJson
instance JSONSchema Result where schema    = gSchema

data DepFailure = DepFailure
  { dfPackageName    :: PackageName
  , dfPackageVersion :: VersionName
  , dfMessage        :: Text
  } deriving (Eq, Generic, Show)
instance ToJSON     DepFailure where toJSON    = gtoJsonWithSettings    $ strip "df"
instance FromJSON   DepFailure where parseJSON = gparseJsonWithSettings $ strip "df"
instance JSONSchema DepFailure where schema    = gSchemaWithSettings    $ strip "df"
