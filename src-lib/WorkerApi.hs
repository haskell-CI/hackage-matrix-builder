{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE StrictData        #-}

module WorkerApi where

import Servant.API

import Job
import PkgId
import Prelude.Local

type WorkerApi m =
    -- GET /info
         "info" :> Get '[JSON] WorkerInfo
    -- /jobs
    :<|> "jobs" :> Get '[JSON] JobsInfo
    :<|> "jobs" :> ReqBody '[JSON] CreateJobReq        :> PostCreated '[JSON] CreateJobRes -- TODO: use PostCreated
    :<|> "jobs" :> Capture "jid" JobId :> "solve"      :> Get '[JSON] JobSolve
    :<|> "jobs" :> Capture "jid" JobId :> "build_deps" :> Get '[JSON] JobBuildDeps
    :<|> "jobs" :> Capture "jid" JobId :> "build"      :> Get '[JSON] JobBuild
    :<|> "jobs" :> Capture "jid" JobId                 :> DeleteNoContent '[JSON] NoContent -- TODO: use DeleteNoContent


type JobId = Word

data WorkerInfo = WorkerInfo
    { wiUptime       :: Word
    , wiOsArch       :: (String,String)
    , wiGhcVersions  :: [Ver]
    , wiJobQueueSize :: Word
    , wiIndexTs      :: Word
    -- TODO: os_release info
    } deriving (Generic, Show)

data CreateJobReq = CreateJobReq
    { cjrqGhcVersion :: Ver
    , cjrqIndexTs    :: Maybe Word
    , cjrqPkgId      :: PkgId
    } deriving Generic

data CreateJobRes = CreateJobRes
    { cjrsJobId :: JobId
    } deriving (Generic,Show)

instance ToJSON   WorkerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON WorkerInfo where { parseJSON = myParseJSON }

instance ToJSON   CreateJobReq where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobReq where { parseJSON = myParseJSON }

instance ToJSON   CreateJobRes where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON CreateJobRes where { parseJSON = myParseJSON }

type JobsInfo = [JobId]

-- instance ToJSON JobsInfo
-- instance FromJSON JobsInfo

data JobSolve = JobSolve
    { jpFetch :: Maybe JobStep
    , jpSolve :: Maybe JobStep
    } deriving (Generic, Show)

instance ToJSON   JobSolve where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobSolve where { parseJSON = myParseJSON }

data JobBuildDeps = JobBuildDeps
    { jrFetchDeps :: Maybe JobStep
    , jrBuildDeps :: Maybe JobStep
    } deriving (Generic, Show)

instance ToJSON   JobBuildDeps where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobBuildDeps where { parseJSON = myParseJSON }

data JobBuild = JobBuild
    { jrBuild     :: Maybe JobStep
    } deriving (Generic, Show)

instance ToJSON   JobBuild where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobBuild where { parseJSON = myParseJSON }

