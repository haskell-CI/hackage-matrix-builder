module Types where

import Prelude (Unit, unit, pure, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (ready)
import Data.Set (Set)
import DOM (DOM)
import Control.Monad.Trans ()
import Data.List (List)
import Data.Map (Map)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Date (Date)

data Tags = Tags { unTags :: Map TagName (Set PackageName) }

type Tag =
  { name     :: TagName
  , packages :: Set PackageName
  }

data PackageMeta = PackageMeta
  { pmName   :: PackageName
  , pmReport :: Maybe Date
  , pmTags   :: Set TagName
  }

data ReportMeta = ReportMeta
  { rmPackageName :: PackageName
  , rmModified    :: Date
  }

data Report = Report
  { rPackageName :: PackageName
  , rModified    :: Date
  , rResults     :: List GHCResult
  }

data ShallowReport = ShallowReport
 { sPackageName :: PackageName
 , sModified    :: Date
 , sResults     :: List ShallowGhcResult
 }

data ShallowGhcResult = ShallowGhcResult
  { sGhcVersion     :: VersionName
  , sGhcFullVersion :: VersionName
  , sGhcResult      :: List ShallowVersionResult
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
  , resultsA       :: List VersionResult
  , resultsB       :: List (Tuple PkgVerPfx (Maybe VersionName))
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
  | FailDeps (List DepFailure)

data DepFailure = DepFailure
  { dfPackageName    :: PackageName
  , dfPackageVersion :: VersionName
  , dfMessage        :: String
  }

data Package = Package
  { pName     :: PackageName
  , pVersions :: List VersionInfo
  }

type Username = String

type User =
  { name     :: Username
  , packages :: List PackageName
  }

-- TODO Remove prefixes

data HackageUserRep = HackageUserRep
  { hugroups   :: List String
  , huusername :: String
  , huuserid   :: Word
  }

type PackageName = String

type VersionName = String

type Revision = Word

type TagName = String

type PkgVerPfx = List Word

type Word = Int

type ApiList a = { offset :: Int, count :: Int, items :: List a }
