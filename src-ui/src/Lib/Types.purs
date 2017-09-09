module Lib.Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Lib.Undefined (Undefined)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep as G

type Username = String
type PackageName = String
type VersionName = String
type Revision = Word
type TagName = String
type PkgVerPfx = Array Word
type HackageUrl = String
type RevisionUrl = String
type HdiffUrl = String
type Cell = String
type Word = Int
type Prefixs = String
type PkgIdxTs = Int

data PageRoute = HomePage
               | LatestPage
               | PackagesPage
               | PackagePage PackageName
               | LogRoute PackageName VersionName
               | UserPage Username
               | ErrorPage

derive instance gPageRoute :: G.Generic PageRoute _

instance sPageRoute :: Show PageRoute where show = genericShow

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
  , resultsB       :: Array (Tuple PkgVerPfx (Undefined VersionName))
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

-- TODO Remove prefixes

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
