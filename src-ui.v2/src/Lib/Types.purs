module Lib.Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep as G
import Data.StrMap as SM
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Tuple.Nested as TupleN


type InitialPackage = TupleN.Tuple4 PackageName PackageTS (Maybe VersionName) (Maybe HCVer)
type Username = String
type PackageName = String
type VersionName = String
type Revision = Int
type TagName = String
type TagsWithPackages = SM.StrMap (Array PackageName)
type PkgVerPfx = Array Int
type HackageUrl = String
type RevisionUrl = String
type HdiffUrl = String
type Cell = String
type Word = Int
type PkgIdxTs = Int
type PackageTS = String
type HCVer = String

type CellReportType = String
type BuildStatus = String
type BuildLog = String
type UUID = String

data PageRoute = HomePage
               | LatestPage
               | PackagesPage
               | PackagePage PackageName
               | PackagePageVersion PackageName VersionName HCVer
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

pkgIdxTsReportsEmpty :: PackageIdxTsReports
pkgIdxTsReportsEmpty =
  {
    pkgname: ""
  , idxstate: 0
  , hcversions: []
  , pkgversions: SM.empty
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

cellReportSummaryDefault :: CellReportSummary
cellReportSummaryDefault =
  {
    crsT: ""
  , crsBjle: 0
  , crsPerr: false
  , crsBok: 0
  , crsBfail: 0
  , crsBdfail: 0
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
type PackageHistory = TupleN.Tuple4 PkgIdxTs VersionName Revision Username
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

unitIdInfoEmpty :: UnitIdInfo
unitIdInfoEmpty =
  {
    uiiId: ""
  , uiiHcVer: ""
  , uiiPkgname: ""
  , uiiPkgver: ""
  , uiiStatus: ""
  , uiiLogmsg: ""
  , uiiLibDeps: SM.empty
  , uiiExeDeps: SM.empty
  }

-- /v2/units/{unitid}/deps
type UnitIdInfoDeps = SM.StrMap UnitIdInfoDep
type UnitIdInfoDep =
  {
    pkgname :: PackageName
  , pkgver :: VersionName
  , status :: BuildStatus
  , lib_deps :: SM.StrMap (Array UUID)
  , exe_deps :: SM.StrMap (Array UUID)
  }

unitIdInfoDepEmpty :: UnitIdInfoDep
unitIdInfoDepEmpty =
  {
    pkgname: ""
  , pkgver: ""
  , status: ""
  , lib_deps: SM.empty
  , exe_deps: SM.empty
  }
-- /v2/queue || /v2/queue/{pkgname} || /v2/queue/{pkgname}/{idxstate}
type PackageQueue =
  {
    priority :: Int
  , modified :: PackageTS
  , pkgname :: PackageName
  , idxstate :: PkgIdxTs
  }

type ColumnVersion =
  { ghcVer :: Maybe VersionName
  , pkgVer :: Maybe VersionName
  }

type User =
  { name     :: Username
  , packages :: Array PackageName
  }

userEmpty :: User
userEmpty =
  { name : ""
  , packages : []
  }

