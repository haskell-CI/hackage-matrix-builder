{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Controller.Api where

import           HackageApi                           (UserName)
import           PkgId
import           Prelude.Local

import           Data.Set (Set)
import qualified Data.Aeson                           as J
import qualified Data.Aeson.Types                     as J
import           Data.Char
import qualified Database.PostgreSQL.Simple           as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Servant.API

type ControllerApi m =
  -- legacy rest-core style API
       "v1.0.0" :> "tag"                 :> "list" :> ListOp TagListEntry
  :<|> "v1.0.0" :> "tag"                 :> "name" :> Capture "tagname" TagName :> ReqBody '[JSON] PkgN :> Put '[JSON] ()
  :<|> "v1.0.0" :> "tag"                 :> "name" :> Capture "tagname" TagName :> ReqBody '[JSON] PkgN :> Delete '[JSON] ()

  :<|> "v1.0.0" :> "package"             :> "list" :> ListOp PkgListEntry
  :<|> "v1.0.0" :> "package"             :> "list-latest-reports" :> ListOp PkgListEntry2
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> Get '[JSON] PkgVerInfo
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "tags" :> Post '[JSON] [TagName]
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "latest" :> Get '[JSON] JobReport
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "latest" :> "cell" :> "id" :> Capture "cellid" Text :> Get '[JSON] CellReport

  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "idxstate" :> Capture "idxstate" PkgIdxTs :> Get '[JSON] JobReport
  :<|> "v1.0.0" :> "package"             :> "name" :> Capture "pkgname" PkgN :> "report" :> "idxstate" :> Capture "idxstate" PkgIdxTs :> "cell" :> "id" :> Capture "cellid" Text :> Get '[JSON] CellReport

  :<|> "v1.0.0" :> "queue"               :> "list"                                                     :> ListOp QEntry
  :<|> "v1.0.0" :> "queue"                                                   :> ReqBody '[JSON] QEntry :> Post '[JSON] ()
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN                           :> Get '[JSON] QEntry
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN :> ReqBody '[JSON] QPrio  :> Put '[JSON] QEntry
  :<|> "v1.0.0" :> "queue"               :> "name" :> Capture "pkgname" PkgN                           :> Delete '[JSON] ()

  :<|> "v1.0.0" :> "user"                :> "name" :> Capture "username" UserName                      :> Get '[JSON] UserPkgs

  -- New-style API; we stick w/ the more common RESTful convention of using plural nouns for listable collections
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "reports" :> Get '[JSON] (Set PkgIdxTs)

type ListOp e = QueryParam "count" Word :> Post '[JSON] (ListSlice e)

type TagName = Text

data TagListEntry = TagListEntry
    { teName     :: !TagName
    , tePackages :: [PkgN]
    } deriving (Generic)

data PkgListEntry = PkgListEntry
    { pleName   :: !PkgN
    , pleTags   :: [TagName]
    , pleReport :: Maybe UTCTime
    } deriving (Generic)

data PkgListEntry2 = PkgListEntry2
    { ple2PackageName :: !PkgN
    , ple2Modified    :: !UTCTime
    } deriving (Generic,Eq,Ord,Show)

data ListSlice a = ListSlice
    { lsOffset :: !Word
    , lsCount  :: !Word
    , lsItems  :: [a]
    } deriving (Generic)

data PkgVerInfo = PkgVerInfo
    { pviName     :: PkgN
    , pviVersions :: [PkgVerInfoEntry]
    } deriving (Generic)

data PkgVerInfoEntry = PkgVerInfoEntry
    { pvieVersion    :: !Ver
    , pvieRevision   :: !Word
    , pviePreference :: !Text
    } deriving (Generic,Eq,Ord)

data QPrio = QPlow
           | QPmedium
           | QPhigh
           deriving (Generic,Eq,Ord,Enum,Bounded,Show)

instance FromField QPrio where
    fromField f mdata = fromInt <$> fromField f mdata
      where
        fromInt :: Int -> QPrio
        fromInt i
          | i >=  10   = QPhigh
          -- medium
          | i <= -10   = QPlow

          | otherwise  = QPmedium


instance ToField QPrio where
    toField = toField . toInt
      where
        toInt :: QPrio -> Int
        toInt = \case
            QPhigh   ->  10
            QPmedium ->   0
            QPlow    -> -10

data QEntry = QEntry
    { qPriority    :: QPrio
    , qModified    :: Maybe UTCTime
    , qPackageName :: PkgN
    , qIdxState    :: Maybe PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow QEntry

instance ToJSON a => ToJSON (ListSlice a) where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON a => FromJSON (ListSlice a) where { parseJSON = myParseJSON }

instance ToJSON   TagListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON TagListEntry where { parseJSON = myParseJSON }

instance ToJSON   QPrio where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON QPrio where { parseJSON = myParseJSON }

instance ToJSON   QEntry where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntry where { parseJSON = myParseJSONCml }

instance ToJSON PkgListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToJSON PkgListEntry2 where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }

instance ToJSON PkgVerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToJSON PkgVerInfoEntry where { toJSON = myToJSON; toEncoding = myToEncoding }



data JobReport = JobReport
 { jrPackageName :: PkgN
 , jrModified    :: UTCTime
 , jrResults     :: [JobResult]
 } deriving (Eq, Show, Generic)

instance ToJSON   JobReport where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobReport where { parseJSON = myParseJSONCml }

data JobResult = JobResult
  { jrGhcVersion     :: Ver
  , jrGhcFullVersion :: Ver
  , jrGhcResult      :: [JobGhcResult]
  } deriving (Eq, Generic, Show)

instance ToJSON   JobResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobResult where { parseJSON = myParseJSONCml }

data JobGhcResult = JobGhcResult
  { jgrPackageVersion  :: Ver
  , jgrPackageRevision :: Int
  , jgrResult          :: JobResultType
  } deriving (Eq, Generic, Show)

instance ToJSON   JobGhcResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobGhcResult where { parseJSON = myParseJSONCml }

data JobResultType
    = JRTOk
    | JRTNop
    | JRTNoIp
    | JRTNoIpBjLimit Word
    | JRTNoIpFail
    | JRTFail
    | JRTFailDeps Word
    deriving (Eq, Generic, Show)

instance ToJSON   JobResultType where { toJSON = J.genericToJSON jobResOpts }
instance FromJSON JobResultType where { parseJSON = J.genericParseJSON jobResOpts }

data CellReport = CellReport
  { crGhcVersion     :: Ver
  , crGhcFullVersion :: Ver
  , crLogMsg         :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON CellReport where
    toJSON CellReport{..} =
        J.object [ "ghcVersion" J..= crGhcVersion
                 , "ghcFullVersion" J..= crGhcFullVersion
                 , "resultA" J..=
           J.object [ "result" J..=
              J.object [ "fail" J..= crLogMsg ] ] ]

           -- [ "ghcVersion" J..= crGhcVersion ]


jobResOpts :: J.Options
jobResOpts = J.defaultOptions { J.sumEncoding = J.ObjectWithSingleField
                              , J.constructorTagModifier = labelModCml }
  where
    labelModCml = uncap . drop 3

    uncap []     = []
    uncap (c:cs) = toLower c : cs



data UserPkgs = UserPkgs
  { upName     :: UserName
  , upPackages :: Set PkgN
  } deriving (Eq, Generic, Show)

instance ToJSON   UserPkgs where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON UserPkgs where { parseJSON = myParseJSONCml }
