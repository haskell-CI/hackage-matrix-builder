module Lib.Types where

import Data.Eq
import Data.Generic
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Lib.Undefined

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

data ReportMeta = ReportMeta
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

type Username = String

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
  { packageName :: PackageName
  , modified :: String
  , priority :: Priority
  }
