module Types where

-- import Data.Set (Set)
-- import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Date (Date)

data Tags = Tags { unTags :: Map TagName (Array PackageName) }

type Tag =
  { name     :: TagName
  , packages :: Array PackageName
  }

type PackageMeta =
  { name   :: PackageName
  , report :: Maybe Date
  , tags   :: Array TagName
  }

data ReportMeta = ReportMeta
  { rmPackageName :: PackageName
  , rmModified    :: Date
  }

data Report = Report
  { rPackageName :: PackageName
  , rModified    :: Date
  , rResults     :: Array GHCResult
  }

data ShallowReport = ShallowReport
 { sPackageName :: PackageName
 , sModified    :: Date
 , sResults     :: Array ShallowGhcResult
 }

data ShallowGhcResult = ShallowGhcResult
  { sGhcVersion     :: VersionName
  , sGhcFullVersion :: VersionName
  , sGhcResult      :: Array ShallowVersionResult
  }

data ShallowVersionResult = ShallowVersionResult
 { sPackageVersion  :: VersionName
 , sPackageRevision :: Revision
 , sResult          :: ShallowResult
 }

data ShallowResult
  = ShallowOk
  | ShallowNop
  | ShallowNoIp
  | ShallowNoIpBjLimit Word
  | ShallowNoIpFail
  | ShallowFail
  | ShallowFailDeps Word


data VersionInfo = VersionInfo
  { version    :: VersionName
  , revision   :: Revision
  , preference :: Preference
  }

data Preference
  = Normal
  | UnPreferred
  | Deprecated


data GHCResult = GHCResult
  { ghcVersion     :: VersionName
  , ghcFullVersion :: VersionName
  , resultsA       :: Array VersionResult
  , resultsB       :: Array (Tuple PkgVerPfx (Maybe VersionName))
  }

data SingleResult = SingleResult
  { srGhcVersion     :: VersionName
  , srGhcFullVersion :: VersionName
  , srResultA        :: Maybe VersionResult
  }

data VersionResult = VersionResult
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

data DepFailure = DepFailure
  { dfPackageName    :: PackageName
  , dfPackageVersion :: VersionName
  , dfMessage        :: String
  }

data Package = Package
  { pName     :: PackageName
  , pVersions :: Array VersionInfo
  }

type Username = String

type User =
  { name     :: Username
  , packages :: Array PackageName
  }

-- TODO Remove prefixes

data HackageUserRep = HackageUserRep
  { hugroups   :: Array String
  , huusername :: String
  , huuserid   :: Word
  }

type PackageName = String

type VersionName = String

type Revision = Word

type TagName = String

type PkgVerPfx = Array Word

type Word = Int

type ApiList a = { offset :: Int, count :: Int, items :: Array a }
