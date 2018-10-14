{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Controller.Api
    ( swaggerDoc
    , ControllerApi

    , PkgIdxTs, fmtPkgIdxTs
    , PkgN
    , Ver -- , verToText
    , UserName
    , PkgRev

    , ControllerInfo(..)
    , CompilerInfo(..)

    , UserPkgs(..)

    , TagName
    , TagsInfo(..)

    , QEntryRow(..)
    , QEntryUpd(..)
    , WorkerRow(..)
    , WState(..)

    , PkgHistoryEntry(..)

    , PkgIdxTsReport(..)
    , CellReportType(..)
    , CellReportSummary(..)
    , CellReportDetail(..)
    , IPStatus(..)
    , UnitIdInfo(..)
    , UnitIdTree(..)
    ) where

import           Prelude.Local
import           Util.WebSvc

import           HackageApi                           (UserName (..))
import           PkgId
import qualified PkgIdxTsSet

import           Data.Data.Lens
import           Data.Set                             (Set)
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
    & Swag.schemes                ?~ [Swag.Http,Swag.Https]

    -- Authentication
    -- Simplified scheme: GET doesn't require auth; DELETE & PUT require auth; POST may or may not requires auth...
    & Swag.securityDefinitions    .~ [("basicAuth", Swag.SecurityScheme Swag.SecuritySchemeBasic Nothing)]
    & Swag.paths . traversed . Swag.delete . _Just . Swag.security .~ basicAuth
    & Swag.paths . traversed . Swag.put    . _Just . Swag.security .~ basicAuth

    -- Grouping
    & Swag.applyTagsFor (v2Ops . pathOps) ["V2 API"]
  where
    v2Ops :: Traversal' Swag.Swagger Swag.PathItem
    v2Ops = Swag.paths . itraversed . indices (isPrefixOf "/v2/")

    basicAuth = [Swag.SecurityRequirement [("basicAuth",[])]]

    pathOps :: Traversal' Swag.PathItem Swag.Operation
    pathOps = template

type ControllerApi m =
  -- New-style API; we stick w/ the more common RESTful convention of using plural nouns for listable collections
       "v2" :> "info" :> Get '[JSON] ControllerInfo -- static meta-information

  :<|> "v2" :> "idxstates" :> QueryParam "min" PkgIdxTs :> QueryParam "max" PkgIdxTs :> Get '[JSON] PkgIdxTsSet.PkgIdxTsSet
  :<|> "v2" :> "idxstates" :> "latest" :> Get '[JSON] PkgIdxTs

  :<|> "v2" :> "packages" :> Get '[JSON] (Vector PkgN)
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "tags" :> Get '[JSON] (Set TagName)
  :<|> "v2" :> "packages" :> "*"                    :> "reports" :> "latest" :> Get '[JSON] (Map PkgN PkgIdxTs)
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "reports" :> Get '[JSON] (Set PkgIdxTs)
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "reports" :> Capture "idxstate" PkgIdxTs :> Get '[JSON] PkgIdxTsReport
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "reports" :> Capture "idxstate" PkgIdxTs :> Capture "pkgver" Ver :> Capture "hcver" CompilerID :> Get '[JSON] CellReportDetail
  :<|> "v2" :> "packages" :> Capture "pkgname" PkgN :> "history" :> Get '[JSON] (Vector PkgHistoryEntry)

  :<|> "v2" :> "units" :> Capture "unitid" UUID :> Get '[JSON] UnitIdInfo
  :<|> "v2" :> "units" :> Capture "unitid" UUID :> "deps" :> Get '[JSON] (Map UUID UnitIdTree)

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

  :<|> "v2" :> "users" :> "name"                 :> Capture "username" UserName :> Get '[JSON] UserPkgs

  :<|> "v2" :> "workers" :> Get '[JSON] [WorkerRow]

----------------------------------------------------------------------------

type TagName = Text

data TagsInfo = TagsInfo     (Set TagName)
              | TagsInfoPkgs (Map TagName (Set PkgN))
              deriving Generic

instance ToSchema TagsInfo -- FIXME

instance ToJSON TagsInfo where
    toJSON (TagsInfo x)     = toJSON x
    toJSON (TagsInfoPkgs x) = toJSON x

----------------------------------------------------------------------------

data ControllerInfo = ControllerInfo
     { ciCompilers :: Map CompilerID CompilerInfo
     } deriving (Generic,Eq,Ord)

instance ToJSON   ControllerInfo where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON ControllerInfo where { parseJSON = myParseJSON }
instance ToSchema ControllerInfo where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   ControllerInfo
instance Hashable ControllerInfo

data CompilerInfo = CompilerInfo
    { ciActive  :: !Bool
    , ciUiLabel :: Maybe Text
    } deriving (Generic,Eq,Ord)

instance ToJSON   CompilerInfo where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON CompilerInfo where { parseJSON = myParseJSON }
instance ToSchema CompilerInfo where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   CompilerInfo
instance Hashable CompilerInfo

----------------------------------------------------------------------------

-- TODO: use PkgRev -- needs 'FromField PkgRev' instance
data PkgHistoryEntry = PkgHistoryEntry !PkgIdxTs !Ver !Int !UserName
                     deriving (Generic,Eq,Ord)
    -- { pheIdxState :: !PkgIdxTs
    -- , pheVersion  :: !Ver
    -- , pheRevision :: !Int
    -- , pheUsername :: !UserName
    -- } deriving (Generic,Eq,Ord)

instance PGS.FromRow PkgHistoryEntry


-- V2 API
data QEntryRow = QEntryRow
    { qrPriority :: Int
    , qrModified :: UTCTime
    , qrPkgname  :: PkgN
    , qrIdxstate :: PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow QEntryRow
instance Hashable QEntryRow where
    hashWithSalt s (QEntryRow p m n i) = hashWithSalt s (p,m',n,i)
        where
          m' = realToFrac (utcTimeToPOSIXSeconds m) :: Double

-- | Subset of 'QEntryRow' which can be set/updated via PUT request
data QEntryUpd = QEntryUpd
    { quPriority :: Int
    } deriving (Generic,Eq,Ord,Show)


-- (wid,mtime,wstate,pname,pver,ptime,compiler)
data WState = WSidle
            | WSinit
            | WSsolve
            | WSbuilddeps
            | WSbuild
            | WSdone
            | WSerror
            deriving (Eq,Ord,Show,Generic)

instance NFData WState
instance Hashable WState
instance ToJSON   WState where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON WState where { parseJSON = myParseJSON }
instance ToSchema WState where { declareNamedSchema = myDeclareNamedSchema }

instance FromField WState where
    fromField _f mdata = return (go mdata) -- FIXME, check type
      where
        go (Just "idle")       = WSidle
        go (Just "init")       = WSinit
        go (Just "solve")      = WSsolve
        go (Just "build-deps") = WSbuilddeps
        go (Just "build")      = WSbuild
        go (Just "done")       = WSdone
        go (Just "error")      = WSerror
        go _                   = error ("FromField(WState) " ++ show mdata)

data WorkerRow = WorkerRow
    { wrId         :: !Int
    , wrModified   :: !Int -- unix timestamp; fixme, convert to UTCTime
    , wrState      :: !WState
    , wrPkgname    :: Maybe PkgN
    , wrPkgversion :: Maybe Ver
    , wrIdxState   :: Maybe PkgIdxTs
    , wrHcversion  :: Maybe CompilerID
    } deriving (Generic,Eq,Ord,Show)

instance PGS.FromRow WorkerRow
instance Hashable WorkerRow
instance ToJSON   WorkerRow where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON WorkerRow where { parseJSON = myParseJSONCml }
instance ToSchema WorkerRow where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   QEntryRow where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntryRow where { parseJSON = myParseJSONCml }
instance ToSchema QEntryRow where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   QEntryUpd where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON QEntryUpd where { parseJSON = myParseJSONCml }
instance ToSchema QEntryUpd where { declareNamedSchema = myDeclareNamedSchemaCml }

instance ToJSON   PkgHistoryEntry where { toJSON = myToJSON; toEncoding = myToEncoding }
instance ToSchema PkgHistoryEntry where
    declareNamedSchema p = do -- (        & (example ?~ toJSON (PkgIdxTs 1491048000)
        s <- myDeclareNamedSchema p
        pure $ s & Swag.schema.Swag.description ?~ "Package history entry represented as 4-tuple (idxstate,version,revision,username)"
                 & Swag.schema.Swag.example     ?~ toJSON (PkgHistoryEntry (PkgIdxTs 1343543615) (mkVer (1 :| [0])) 0 (UserName "EdwardKmett"))


data UserPkgs = UserPkgs
  { upName     :: UserName
  , upPackages :: Set PkgN
  } deriving (Eq, Generic, Show)

instance ToJSON   UserPkgs where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON UserPkgs where { parseJSON = myParseJSONCml }
instance ToSchema UserPkgs where { declareNamedSchema = myDeclareNamedSchemaCml }

-- v2 report
data PkgIdxTsReport = PkgIdxTsReport
    { pitrPkgname     :: PkgN
    , pitrIdxstate    :: PkgIdxTs
    , pitrHcversions  :: [CompilerID]
    , pitrPkgversions :: Map Ver [CellReportSummary] -- invariant: len(pitrPkgVersions) == len(pitrPkgVersions[v]) forall v
    } deriving (Generic)

data CellReportType = CRTna | CRTpf | CRTse
                    deriving (Generic,Show,Eq)

data CellReportSummary = CellReportSummary
    { crsT      :: Maybe CellReportType -- 'Nothing' means "not available"
    -- field for 'CRTpf' -- plain-fail
    , crsBjle   :: Maybe Word -- BJ limit exhausted
    , crsPerr   :: Maybe Bool -- other plan error
    -- fields for 'CRTse' -- install plan solution exists
    , crsBok    :: Maybe Word -- build ok
    , crsBfail  :: Maybe Word -- build fails
    , crsBdfail :: Maybe Word -- dep build fails
    } deriving (Generic,Show)

data CellReportDetail = CellReportDetail
    { crdPkgname    :: PkgN
    , crdPkgversion :: Ver
    , crdIdxstate   :: PkgIdxTs
    , crdHcversion  :: CompilerID
    , crdType       :: CellReportType
      -- CRTpf
    , crdSolverErr  :: Maybe Text
      -- CRTse
    , crdUnits      :: Maybe [Map UUID (Maybe IPStatus)]
    } deriving (Generic,Show)

instance ToJSON   PkgIdxTsReport where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON PkgIdxTsReport where { parseJSON = myParseJSON }
instance ToSchema PkgIdxTsReport where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   PkgIdxTsReport
instance Hashable PkgIdxTsReport

instance ToJSON   CellReportType where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON CellReportType where { parseJSON = myParseJSON }
instance ToSchema CellReportType where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   CellReportType
instance Hashable CellReportType

instance ToJSON   CellReportSummary where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON CellReportSummary where { parseJSON = myParseJSON }
instance ToSchema CellReportSummary where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   CellReportSummary
instance Hashable CellReportSummary

instance ToJSON   CellReportDetail where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON CellReportDetail where { parseJSON = myParseJSON }
instance ToSchema CellReportDetail where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   CellReportDetail
instance Hashable CellReportDetail

data UnitIdInfo = UnitIdInfo
    { uiiId      :: UUID
--    , uiiUnitid :: UnitID
    , uiiHcver   :: CompilerID
    , uiiPkgname :: PkgN
    , uiiPkgver  :: Ver
--    , uuiFlags   :: J.Value
    , uiiStatus  :: Maybe IPStatus
    , uiiLogmsg  :: Maybe Text
--    , uiiDt

    , uiiLibDeps :: Map Text (Set UUID)
    , uiiExeDeps :: Maybe (Map Text (Set UUID))
    } deriving (Generic,Show)

instance ToJSON   UnitIdInfo where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON UnitIdInfo where { parseJSON = myParseJSON }
instance ToSchema UnitIdInfo where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   UnitIdInfo
instance Hashable UnitIdInfo

-- | This is a bit of a subset of 'UnitIdInfo', and intended to be used
-- as @Map UUID UnitIdTree@ to provide a dependency DAG.
data UnitIdTree = UnitIdTree
    { uitPkgname :: PkgN
    , uitPkgver  :: Ver
    , uitStatus  :: Maybe IPStatus
    , uitLibDeps ::        Map Text (Set UUID)
    , uitExeDeps :: Maybe (Map Text (Set UUID))
    } deriving (Generic,Show)

instance ToJSON   UnitIdTree where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON UnitIdTree where { parseJSON = myParseJSON }
instance ToSchema UnitIdTree where { declareNamedSchema = myDeclareNamedSchema }
instance NFData   UnitIdTree
instance Hashable UnitIdTree

-- | Build-status for a build-unit
data IPStatus = IPOk
              | IPBuildFail
              | IPBuildDepsFail
              deriving (Eq,Ord,Show,Generic)

instance NFData IPStatus
instance Hashable IPStatus

instance ToField IPStatus where
    toField = toField . go
      where
        go :: IPStatus -> Text
        go = \case
            IPOk            -> "ok"
            IPBuildFail     -> "fail"
            IPBuildDepsFail -> "fail_deps"

instance FromField IPStatus where
    fromField _f mdata = return (go mdata) -- FIXME, check type
      where
        go (Just "ok")        = IPOk
        go (Just "fail")      = IPBuildFail
        go (Just "fail_deps") = IPBuildDepsFail
        go _                  = error ("FromField(IPStatus) " ++ show mdata)


instance ToJSON IPStatus where
    toJSON x = toJSON $ case x of
                          IPOk            -> "bok" :: Text
                          IPBuildFail     -> "bfail"
                          IPBuildDepsFail -> "bdfail"

instance FromJSON IPStatus where
    parseJSON o = do
        x <- parseJSON o

        case (x :: Text) of
          "bok"    -> pure IPOk
          "bfail"  -> pure IPBuildFail
          "bdfail" -> pure IPBuildDepsFail
          _        -> fail "IPStatus"

instance ToSchema IPStatus where
    declareNamedSchema _ = pure $ Swag.NamedSchema (Just "IPStatus") $ mempty
        & Swag.type_ .~ Swag.SwaggerString
        & Swag.example ?~ toJSON IPOk
        & Swag.description ?~ "build status (one of `\"bok\"`, `\"bfail\"` or `\"bdfail\"`)"
        & Swag.enum_ ?~ map toJSON [IPOk,IPBuildFail,IPBuildDepsFail]
