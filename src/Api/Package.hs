{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Api.Package where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson             (FromJSON (..), ToJSON (..), decode)
import qualified Data.ByteString.Lazy   as L
import           Data.JSON.Schema
import qualified Data.Map.Strict        as Map
import           Data.String.ToString
import           Data.Text              (pack, unpack)
import qualified Data.Text              as T
import           Generics.Generic.Aeson
import           Rest
import           Rest.Info              (Info (..))
import qualified Rest.Resource          as R
import           Rest.ShowUrl
import           System.Directory

import           Api.Types
import           BuildReport
import           BuildTypes

data Identifier = Name Text

instance Info Identifier where describe _ = "identifier"

instance ShowUrl Identifier where
  showUrl (Name t) = unpack t

type WithPackage = ReaderT Identifier Root

resource :: Resource Root WithPackage Identifier () Void
resource = mkResourceReader
  { R.name   = "package"
  , R.schema = withListing () $ named [("name", singleBy (Name . pack))]
  , R.get    = Just get
  , R.list   = const list
  }

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [Text]
    handler _ =
      map pack . map (reverse . drop 5) . filter ((reverse ".json" ==) . take 5) . map reverse <$> liftIO (getDirectoryContents "report/")

get :: Handler WithPackage
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithPackage ReportDataJson
    handler = do
      Name t <- ask
      let fp = "report/" ++ unpack t ++ ".json"
      exists <- liftIO $ doesFileExist fp
      unless exists $ throwError NotFound
      f <- liftIO $ L.readFile fp
      maybe (throwError Busy) (return . reportDataJson) . decode $ f

data ReportDataJson = ReportDataJson
    { pkgName     :: String
    , versions    :: [Ver]
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
      , versions    = map toVer . Map.toList $ b
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
        { ghcVer = ghcVerToV w
        , pkgVer = pkgVerToV x
        , resultsA = map (\(a1,a2) -> VersionResult { pkgVersion = pkgVerToV a1, result = br a2 }) . Map.toList $ y
        , resultsB = map (second $ fmap pkgVerToV) . Map.toList $ z
        }
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

pkgVerToV :: PkgVer -> V
pkgVerToV p = V
  { segments = unPkgVer p
  , name     = T.unpack . tshowPkgVer $ p
  }

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
