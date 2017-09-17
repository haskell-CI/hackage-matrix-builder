{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
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
import qualified Data.Swagger                         as Swag
import qualified Database.PostgreSQL.Simple           as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Servant.API
import           Servant.Swagger                      as Swag


swaggerDoc :: Swag.Swagger
swaggerDoc = toSwagger (Proxy :: Proxy (ControllerApi ()))
    & Swag.info.Swag.title        .~ "Hackage Matrix Builder Controller API"
    & Swag.info.Swag.version      .~ "3"
    & Swag.basePath               ?~ "/api"
    & Swag.schemes                ?~ [Swag.Http]
    -- Simplified scheme: GET doesn't require auth; DELETE & PUT require auth; POST may or may not requires auth...
    & Swag.securityDefinitions    .~ [("basicAuth", Swag.SecurityScheme Swag.SecuritySchemeBasic Nothing)]
    & Swag.paths . traversed . Swag.delete . _Just . Swag.security .~ basicAuth
    & Swag.paths . traversed . Swag.put    . _Just . Swag.security .~ basicAuth
    -- so far only a single POST service requires auth
    & Swag.paths . ix "/v1.0.0/queue" . Swag.post . _Just . Swag.security .~ basicAuth
  where
    basicAuth = [Swag.SecurityRequirement [("basicAuth",[])]]

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
  :<|> "v2" :> "idxstates" :> QueryParam "min" PkgIdxTs :> QueryParam "max" PkgIdxTs :> Get '[JSON] [PkgIdxTs]
  :<|> "v2" :> "idxstates" :> "latest" :> Get '[JSON] PkgIdxTs

  :<|> "v2" :> "packages" :> Get '[JSON] [PkgN]
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "tags" :> Get '[JSON] (Set TagName)
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "reports" :> Get '[JSON] (Set PkgIdxTs)
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "history" :> Get '[JSON] [PkgHistoryEntry]

  :<|> "v2" :> "tags" :> QueryFlag "pkgnames" :> Get '[JSON] TagsInfo
  :<|> "v2" :> "tags" :> Capture "tagname" TagName :> Get '[JSON] (Set PkgN)
  :<|> "v2" :> "tags" :> Capture "tagname" TagName :> Capture "pkgname" PkgN :> PutNoContent '[JSON] NoContent
  :<|> "v2" :> "tags" :> Capture "tagname" TagName :> Capture "pkgname" PkgN :> DeleteNoContent '[JSON] NoContent

  :<|> "v2" :> "queue" :>                                                          Get '[JSON] [QEntryRow]
  :<|> "v2" :> "queue" :> Capture "pkgname" PkgN :>                                Get '[JSON] [QEntryRow]
--  :<|> "v2" :> "queue" :> Capture "pkgname" PkgN :> ReqBody '[JSON] QEntryUpd :> Post '[JSON] QEntryRow
  :<|> "v2" :> "queue" :> Capture "pkgname" PkgN :> Capture "idxstate" PkgIdxTs :> Get '[JSON]  QEntryRow
  :<|> "v2" :> "queue" :> Capture "pkgname" PkgN :> Capture "idxstate" PkgIdxTs :> ReqBody '[JSON] QEntryUpd :> Put '[JSON] QEntryRow
  :<|> "v2" :> "queue" :> Capture "pkgname" PkgN :> Capture "idxstate" PkgIdxTs :> DeleteNoContent '[JSON] NoContent

type ListOp e = QueryParam "count" Word :> Post '[JSON] (ListSlice e)

type TagName = Text

data TagsInfo = TagsInfo     (Set TagName)
              | TagsInfoPkgs (Map TagName (Set PkgN))
              deriving Generic

instance ToSchema TagsInfo -- FIXME


instance ToJSON TagsInfo where
    toJSON (TagsInfo x)     = toJSON x
    toJSON (TagsInfoPkgs x) = toJSON x

data TagListEntry = TagListEntry
    { teName     :: !TagName
    , tePackages :: !(Set PkgN)
    } deriving (Generic)

data PkgListEntry = PkgListEntry
    { pleName   :: !PkgN
    , pleTags   :: !(Set TagName)
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

data PkgHistoryEntry = PkgHistoryEntry !PkgIdxTs !Ver !Int !UserName
                     deriving (Generic,Eq,Ord)
    -- { pheIdxState :: !PkgIdxTs
    -- , pheVersion  :: !Ver
    -- , pheRevision :: !Int
    -- , pheUsername :: !UserName
    -- } deriving (Generic,Eq,Ord)

instance PGS.FromRow PkgHistoryEntry

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

-- deprecated, used by V1 API
data QEntry = QEntry
    { qPriority    :: QPrio
    , qModified    :: Maybe UTCTime
    , qPackageName :: PkgN
    , qIdxState    :: Maybe PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow QEntry

-- V2 API
data QEntryRow = QEntryRow
    { qrPriority    :: Int
    , qrModified    :: UTCTime
    , qrPkgname     :: PkgN
    , qrIdxstate    :: PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow QEntryRow

-- | Subset of 'QEntryRow' which can be set/updated via PUT request
data QEntryUpd = QEntryUpd
    { quPriority :: Int
    } deriving (Generic,Eq,Ord,Show)

instance ToJSON   a => ToJSON   (ListSlice a) where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON a => FromJSON (ListSlice a) where { parseJSON = myParseJSON }
instance ToSchema a => ToSchema (ListSlice a) where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   TagListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON TagListEntry where { parseJSON = myParseJSON }
instance ToSchema TagListEntry where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   QPrio where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON QPrio where { parseJSON = myParseJSON }
instance ToSchema QPrio where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   QEntry where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntry where { parseJSON = myParseJSONCml }
instance ToSchema QEntry where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   QEntryRow where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntryRow where { parseJSON = myParseJSONCml }
instance ToSchema QEntryRow where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   QEntryUpd where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntryUpd where { parseJSON = myParseJSONCml }
instance ToSchema QEntryUpd where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   PkgListEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToSchema PkgListEntry where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   PkgListEntry2 where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance ToSchema PkgListEntry2 where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   PkgVerInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToSchema PkgVerInfo where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   PkgVerInfoEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToSchema PkgVerInfoEntry where { declareNamedSchema = myDeclareNamedSchema }

instance ToJSON   PkgHistoryEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToSchema PkgHistoryEntry where
    declareNamedSchema p = do -- (        & (example ?~ toJSON (PkgIdxTs 1491048000)
        s <- myDeclareNamedSchema p
        pure $ s & Swag.schema.Swag.description ?~ "Package history entry represented as 4-tuple (idxstate,version,revision,username)"
                 & Swag.schema.Swag.example     ?~ toJSON (PkgHistoryEntry (PkgIdxTs 1343543615) (mkVer (1 :| [0])) 0 "EdwardKmett")

data JobReport = JobReport
 { jrPackageName :: PkgN
 , jrModified    :: UTCTime
 , jrResults     :: [JobResult]
 } deriving (Eq, Show, Generic)

instance ToJSON   JobReport where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobReport where { parseJSON = myParseJSONCml }
instance ToSchema JobReport where { declareNamedSchema = myDeclareNamedSchemaCml }

data JobResult = JobResult
  { jrGhcVersion     :: Ver
  , jrGhcFullVersion :: Ver
  , jrGhcResult      :: [JobGhcResult]
  } deriving (Eq, Generic, Show)

instance ToJSON   JobResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobResult where { parseJSON = myParseJSONCml }
instance ToSchema JobResult where { declareNamedSchema = myDeclareNamedSchemaCml }

data JobGhcResult = JobGhcResult
  { jgrPackageVersion  :: Ver
  , jgrPackageRevision :: Int
  , jgrResult          :: JobResultType
  } deriving (Eq, Generic, Show)

instance ToJSON   JobGhcResult where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON JobGhcResult where { parseJSON = myParseJSONCml }
instance ToSchema JobGhcResult where { declareNamedSchema = myDeclareNamedSchemaCml }

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

-- fixme
instance ToSchema JobResultType where
    declareNamedSchema _ = pure $ Swag.NamedSchema (Just "JobResultType") $ mempty

jobResOpts :: J.Options
jobResOpts = J.defaultOptions { J.sumEncoding = J.ObjectWithSingleField
                              , J.constructorTagModifier = labelModCml }
  where
    labelModCml = uncap . drop 3

    uncap []     = []
    uncap (c:cs) = toLower c : cs

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

-- fixme
instance ToSchema CellReport where
    declareNamedSchema _ = pure $ Swag.NamedSchema (Just "CellReport") $ mempty

data UserPkgs = UserPkgs
  { upName     :: UserName
  , upPackages :: Set PkgN
  } deriving (Eq, Generic, Show)

instance ToJSON   UserPkgs where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON UserPkgs where { parseJSON = myParseJSONCml }
instance ToSchema UserPkgs where { declareNamedSchema = myDeclareNamedSchemaCml }
