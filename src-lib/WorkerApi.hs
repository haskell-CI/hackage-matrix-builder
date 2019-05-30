{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module WorkerApi where

import qualified Data.Aeson    as J
import           Servant.API

import           Job
import           PkgId
import           Prelude.Local
import           Util.WebSvc

type TsMsg = (Maybe POSIXTime,NonEmpty Text)

type WorkerApi m =
    -- GET /info
         "info" :> Get '[JSON] WorkerInfo
    -- /jobs
    :<|> "jobs" :> Get '[JSON] JobsInfo
    :<|> "jobs" :> ReqBody '[JSON] CreateJobReq        :> PostCreated '[JSON] CreateJobRes
    :<|> "jobs" :> Capture "jid" JobId :> "solve"      :> Get '[JSON] JobSolve
    :<|> "jobs" :> Capture "jid" JobId :> "build_deps" :> Get '[JSON] JobBuildDeps
    :<|> "jobs" :> Capture "jid" JobId :> "build"      :> Get '[JSON] JobBuild
    :<|> "jobs" :> Capture "jid" JobId                 :> DeleteNoContent '[JSON] NoContent

    :<|> "compilers" :> Get '[JSON] [CompilerID]
    :<|> "compilers" :> Capture "compilerid" CompilerID :> "pkgdb" :> "global" :> Get '[JSON] [GPkgInfo]
    :<|> "compilers" :> Capture "compilerid" CompilerID :> "pkgdb" :> "store"  :> Get '[JSON] [SPkgInfo]
    :<|> "compilers" :> Capture "compilerid" CompilerID :> "pkgdb" :> "store"  :> DeleteNoContent '[JSON] NoContent

type JobId = Word

data WorkerInfo = WorkerInfo
    { wiUptime       :: Word
    , wiOsArch       :: (Text,Text)
    , wiGhcVersions  :: [CompilerID]
    , wiJobQueueSize :: Word
    , wiIndexTs      :: PkgIdxTs
    -- TODO: os_release info
    } deriving (Generic, Show)

data CreateJobReq = CreateJobReq
    { cjrqGhcVersion :: CompilerID
    , cjrqIndexTs    :: Maybe PkgIdxTs
    , cjrqPkgId      :: PkgId
    } deriving (Generic,Show)

data CreateJobRes = CreateJobRes
    { cjrsJobId :: JobId
    } deriving (Generic,Show)

instance ToJSON   WorkerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON WorkerInfo where { parseJSON = myParseJSON }
instance NFData   WorkerInfo

instance ToJSON   CreateJobReq where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobReq where { parseJSON = myParseJSON }
instance NFData   CreateJobReq

instance ToJSON   CreateJobRes where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobRes where { parseJSON = myParseJSON }
instance NFData   CreateJobRes

type JobsInfo = [JobId]

-- instance ToJSON JobsInfo
-- instance FromJSON JobsInfo

data JobSolve = JobSolve
    { jpFetch :: Maybe JobStep
    , jpSolve :: Maybe JobStep
    , jpPlan  :: Maybe J.Value
    } deriving (Generic, Show)

instance ToJSON   JobSolve where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobSolve where { parseJSON = myParseJSON }
instance NFData   JobSolve

data JobBuildDeps = JobBuildDeps
    { jrFetchDeps   :: Maybe JobStep
    , jrBuildDeps   :: Maybe JobStep
    , jrBuildLogs   :: Map UnitID Text
    , jrBuildTimes  :: Map UnitID NominalDiffTime
    , jrFailedUnits :: Set UnitID
    } deriving (Generic, Show)

instance ToJSON   JobBuildDeps where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobBuildDeps where { parseJSON = myParseJSON }
instance NFData   JobBuildDeps

data JobBuild = JobBuild
    { jrBuild        :: Maybe JobStep
    , jrBuildLogs2   :: Map UnitID Text
    , jrBuildTimes2  :: Map UnitID NominalDiffTime
    , jrFailedUnits2 :: Set UnitID
    } deriving (Generic, Show)

instance ToJSON   JobBuild where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobBuild where { parseJSON = myParseJSON }
instance NFData   JobBuild

data GPkgInfo = GPkgInfo
     { gpiPkgId   :: PkgId
     , gpiUnitId  :: UnitID
     , gpiLibDeps :: Set UnitID
     } deriving (Show,Generic)

instance ToJSON   GPkgInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON GPkgInfo where { parseJSON = myParseJSON }
instance NFData   GPkgInfo

data SPkgInfo = SPkgInfo
     { spiPkgId   :: PkgId
     , spiUnitId  :: UnitID
     , spiLibDeps :: Set UnitID
     } deriving (Show,Generic)

instance ToJSON   SPkgInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON SPkgInfo where { parseJSON = myParseJSON }
instance NFData   SPkgInfo

