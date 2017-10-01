module Lib.Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe)
import Lib.Undefined (Undefined)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep as G
import Data.StrMap as SM
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Tuple.Nested as TupleN


type Username = String
type PackageName = String
type VersionName = String
type Revision = Word
type TagName = String
type TagsWithPackages = SM.StrMap (Array PackageName)
type PkgVerPfx = Array Word
type HackageUrl = String
type RevisionUrl = String
type HdiffUrl = String
type Cell = String
type Word = Int
type PkgIdxTs = Int
type PackageTS = String
type HCVer = String
type PackageHistory = TupleN.Tuple4 PkgIdxTs VersionName Revision Username
type CellReportType = String
type BuildStatus = String
type BuildLog = String
type UUID = String

data PageRoute = HomePage
               | LatestPage
               | PackagesPage
               | PackagePage PackageName
               | LogRoute PackageName VersionName
               | UserPage Username
               | ErrorPage

derive instance gPageRoute :: G.Generic PageRoute _

instance sPageRoute :: Show PageRoute where show = genericShow

-- API v2 type

-- /v2/packages/{pkgname}/reports/{idxstate}
type PackageIdxTsReports =
  {
    pkgname :: PackageName
  , idxstate :: PkgIdxTs
  , hcversions :: Array HCVer
  , pkgversions :: SM.StrMap (Array CellReportSummary)
  }

type CellReportSummary =
  {
    crsT :: CellReportType
  -- field for CRTpf
  , crsBjle :: Int
  , crsPerr :: Boolean
  -- field for CRTse
  , crsBok :: Int
  , crsBfail :: Int
  , crsBdfail :: Int
  }

-- /v2/packages/{pkgname}/reports/{idxstate}/{pkgver}/{hcver}
type CellReportDetail =
  {
    pkgname :: PackageName
  , pkgversion :: VersionName
  , idxstate :: PkgIdxTs
  , hcversion :: HCVer
  , reporttype :: CellReportType
  , solverE :: String
  , units :: Array (SM.StrMap BuildStatus)
  }

-- /v2/packages/{pkgname}/history
type PackageHistories = Array PackageHistory

-- /v2/units/{unitid}
type UnitIdInfo =
  {
    uiiId :: UUID
  , uiiHcVer :: HCVer
  , uiiPkgname :: PackageName
  , uiiPkgver :: VersionName
  , uiiStatus :: BuildStatus
  , uiiLogmsg :: BuildLog
  , uiiLibDeps :: SM.StrMap (Array UUID)
  , uiiExeDeps :: SM.StrMap (Array UUID)
  }
-- /v2/queue || /v2/queue/{pkgname} || /v2/queue/{pkgname}/{idxstate}
type PackageQueue =
  {
    priority :: Word
  , modified :: PackageTS
  , pkgname :: PackageName
  , idxstate :: PkgIdxTs
  }

-- API v1 type
type PackageState =
  { name :: PackageName
  , index :: PkgIdxTs
  }

data Tags = Tags { unTags :: Map TagName (Array PackageName) }

type Tag =
  { name     :: TagName
  , packages :: Array PackageName
  }

type ColumnVersion =
  { ghcVer :: VersionName
  , pkgVer :: VersionName
  }

type PackageMeta =
  { name   :: PackageName
  , report :: Maybe String
  , tags   :: Array TagName
  }

type ReportMeta =
  { rmPackageName :: PackageName
  , rmModified    :: String
  }

type Report =
  { packageName :: PackageName
  , modified    :: String
  , results     :: Array GHCResult
  }

type GHCResult =
  { ghcVersion     :: VersionName
  , ghcFullVersion :: VersionName
  , results        :: Array VersionResult
  , resultsB       :: Array (Tuple.Tuple PkgVerPfx (Undefined VersionName))
  }

type ShallowReport =
 { packageName :: PackageName
 , modified    :: String
 , results     :: Array ShallowGhcResult
 }

type ShallowGhcResult =
  { ghcVersion     :: VersionName
  , ghcFullVersion :: VersionName
  , ghcResult      :: Array ShallowVersionResult
  }

type ShallowVersionResult =
 { packageVersion  :: VersionName
 , packageRevision :: Revision
 , result          :: ShallowResult
 }

data ShallowResult
  = ShallowOk
  | ShallowNop
  | ShallowNoIp
  | ShallowNoIpBjLimit Word
  | ShallowNoIpFail
  | ShallowFail
  | ShallowFailDeps Word
  | Unknown

data Preference
  = Normal
  | UnPreferred
  | Deprecated

derive instance eqPreference :: Eq Preference

type SingleResult =
  { ghcVersion     :: VersionName
  , ghcFullVersion :: VersionName
  , resultA        :: Maybe VersionResult
  }

type VersionResult =
  { packageVersion  :: VersionName
  , packageRevision :: Revision
  , result          :: Result
  }

data Result
  = Ok
  | Nop
  | NoIp
  | NoIpBjLimit Word
  | NoIpFail { err :: String, out :: String }
  | Fail String
  | FailDeps (Array DepFailure)

type DepFailure =
  { packageName    :: PackageName
  , packageVersion :: VersionName
  , message        :: String
  }

type Package =
  { name     :: PackageName
  , versions :: Array VersionInfo
  }

type VersionInfo =
  { version    :: VersionName
  , revision   :: Revision
  , preference :: Preference
  }

type User =
  { name     :: Username
  , packages :: Array PackageName
  }

type HackageUserRep =
  { groups   :: Array String
  , username :: String
  , userid   :: Word
  }

type ApiList a =
  { offset :: Int
  , count :: Int
  , items :: Array a
  }

type Range =
  { count :: Maybe Int
  , offset :: Maybe Int
  }

data Priority
  = Low
  | Medium
  | High

type QueueItem =
  { priority :: String
  , modified :: String
  , packageName :: PackageName
  }

type LatestItem =
  { packageName :: PackageName
  , modified :: String
  }
