{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module API
    ( PkgIdxTs
    , PkgN
    , Ver, verToText
    , UserName
    , PkgRev

    , QEntryRow(..)
    , QEntryUpd(..)
    , WorkerRow(..)
    , PkgHistoryEntry(..)
    , IdxHistoryEntry(..)

    , PkgIdxTsReport(..)
    , WState(..)
    , IPStatus(..)
    , CellReportType(..)
    , CellReportSummary(..), emptyCellReportSummary, fmtCRS
    , CellReportDetail(..)

    , UnitIdInfo(..)

    , UserPkgs(..)

    , ControllerInfo(..)
    , CompilerInfo(..)
        , getIdxStates
    , getInfo
    , getPackages
    , getPackagesHistory
    , getPackageHistory
    , getPackageReports
    , getPackageReportSummary
    , getPackageReportDetail
    , getPackageTags
    , getQueue
    , getQueuePkg
    , putQueue
    , getTagsPkg
    , getTags
    , putTags
    , deleteTags
    , getUnitInfo
    , getUser
    , getWorkers
    , getWorkersPkg
    ) where

import           PkgId

import           Control.Monad    (fail)
import           Control.Lens
import           Data.Aeson       (FromJSON, ToJSON, decode)
import qualified Data.Aeson       as J
import qualified Data.Aeson.Types as J
import qualified Data.Char        as C
import           Data.Map         (Map)
import           Data.Proxy
import           Data.Set         (Set)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Text.Lazy   (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time        (UTCTime)
import           Data.UUID.Types  (UUID)
import           Data.Vector      (Vector)
import           GHC.Generics     (Rep)
import           Reflex
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

data ControllerInfo = ControllerInfo
    { ciCompilers :: Map CompilerID CompilerInfo
    } deriving (Generic,Eq,Ord)

instance FromJSON ControllerInfo where { parseJSON = myParseJSON }

data CompilerInfo = CompilerInfo
    { ciActive  :: !Bool
    , ciUiLabel :: Maybe Text
    } deriving (Generic,Eq,Ord)

instance FromJSON CompilerInfo where { parseJSON = myParseJSON }

----------------------------------------------------------------------------
data QEntryRow = QEntryRow
    { qrPriority :: Int
    , qrModified :: UTCTime
    , qrPkgname  :: PkgN
    , qrIdxstate :: PkgIdxTs
    } deriving (Generic,Eq,Ord,Show)

-- instance ToJSON   QEntryRow where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance J.FromJSON QEntryRow where { parseJSON = myParseJSONCml }

data QEntryUpd = QEntryUpd
    { quPriority :: Int
    } deriving (Generic,Eq,Ord,Show)

instance J.ToJSON   QEntryUpd where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
-- instance FromJSON QEntryUpd where { parseJSON = myParseJSONCml }
----------------------------------------------------------------------------

data PkgIdxTsReport = PkgIdxTsReport
    { pitrPkgname     :: PkgN
    , pitrIdxstate    :: PkgIdxTs
    , pitrHcversions  :: [CompilerID]
    , pitrPkgversions :: Map Ver [CellReportSummary] -- invariant: len(pitrPkgVersions) == len(pitrPkgVersions[v]) forall v
    } deriving (Generic,Show,Eq)

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
    } deriving (Generic,Show,Eq)

data IPStatus = IPOk
              | IPBuildFail
              | IPBuildDepsFail
              deriving (Eq,Ord,Show,Generic)

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

emptyCellReportSummary :: CellReportSummary
emptyCellReportSummary = CellReportSummary Nothing Nothing Nothing Nothing Nothing Nothing

fmtCRS :: CellReportSummary -> (Text, Text)
fmtCRS CellReportSummary{..} = (T.unwords
    [ ty
    , maybe "" fmtBJ crsBjle
    , maybe "" (("E:"<>) . T.pack . show) crsPerr
    , maybe "" (("O:"<>) . T.pack . show) crsBok
    , maybe "" (("F:"<>) . T.pack . show) crsBfail
    , maybe "" (("D:"<>) . T.pack . show) crsBdfail
    ], cls)
  where
    fmtBJ 2000 = "2k"
    fmtBJ x    = ("J:"<>) . T.pack . show $ x

    (ty,cls) = case crsT of
      Nothing                           -> ("", "stat-unknown")
      Just CRTna                        -> ("NA", "stat-unknown") -- ?
      Just CRTpf | Just _    <- crsBjle -> ("FAIL (BJ)", "stat-fail-bjle")
                 | otherwise            -> ("OK (no-ip)", "stat-ok-no-ip")
      Just CRTse | Just _ <- crsBok
                 , Nothing <- crsBfail
                 , Nothing <- crsBdfail -> ("OK", "stat-ok")
                 | Just _ <- crsBdfail  -> ("FAIL (deps)", "stat-fail-deps")
                 | otherwise            -> ("FAIL", "stat-fail")

instance J.FromJSON IPStatus where
    parseJSON o = do
        x <- J.parseJSON o

        case (x :: Text) of
          "bok"    -> pure IPOk
          "bfail"  -> pure IPBuildFail
          "bdfail" -> pure IPBuildDepsFail
          _        -> fail "IPStatus"

instance J.FromJSON PkgIdxTsReport where { parseJSON = myParseJSON }
instance J.FromJSON CellReportType where { parseJSON = myParseJSON }
instance J.FromJSON CellReportSummary where { parseJSON = myParseJSON }
instance J.FromJSON CellReportDetail where { parseJSON = myParseJSON }

data PkgHistoryEntry = PkgHistoryEntry !PkgIdxTs !Ver !PkgRev !UserName
                     deriving (Generic,Eq,Ord,Show)

instance J.FromJSON PkgHistoryEntry where { parseJSON = myParseJSON }

data IdxHistoryEntry = IdxHistoryEntry !PkgIdxTs !PkgN !Ver !PkgRev !UserName
                     deriving (Generic,Eq,Ord)

instance J.FromJSON IdxHistoryEntry where { parseJSON = myParseJSON }

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

instance FromJSON UnitIdInfo where { parseJSON = myParseJSON }

data WState = WSidle
            | WSinit
            | WSsolve
            | WSbuilddeps
            | WSbuild
            | WSdone
            | WSerror
            deriving (Eq,Ord,Show,Generic)

-- instance ToJSON   WState where { toJSON    = myToJSON; toEncoding = myToEncoding }
instance FromJSON WState where { parseJSON = myParseJSON }

data WorkerRow = WorkerRow
    { wrId         :: !Int
    , wrModified   :: !Int -- unix timestamp; fixme, convert to UTCTime
    , wrState      :: !WState
    , wrPkgname    :: Maybe PkgN
    , wrPkgversion :: Maybe Ver
    , wrIdxState   :: Maybe PkgIdxTs
    , wrHcversion  :: Maybe CompilerID
    } deriving (Generic,Eq,Ord,Show)

-- instance ToJSON   WorkerRow where { toJSON = myToJSONCml; toEncoding = myToEncodingCml }
instance FromJSON WorkerRow where { parseJSON = myParseJSONCml }

data UserPkgs = UserPkgs
  { upName     :: UserName
  , upPackages :: Vector PkgN
  } deriving (Eq, Generic, Show)

instance FromJSON UserPkgs where { parseJSON = myParseJSONCml }
----------------------------------------------------------------------------

myParseJSON, myParseJSONCml :: (Generic a, J.GFromJSON J.Zero (Rep a)) => J.Value -> J.Parser a
myParseJSON = J.genericParseJSON (J.defaultOptions { J.fieldLabelModifier = labelMod, J.constructorTagModifier = tagMod })
myParseJSONCml = J.genericParseJSON (J.defaultOptions { J.fieldLabelModifier = labelModCml })

myToJSON, myToJSONCml :: (Generic a, J.GToJSON J.Zero (Rep a)) => a -> J.Value
myToJSON = J.genericToJSON (J.defaultOptions { J.omitNothingFields = True, J.fieldLabelModifier = labelMod, J.constructorTagModifier = tagMod })
myToJSONCml = J.genericToJSON (J.defaultOptions { J.omitNothingFields = True, J.fieldLabelModifier = labelModCml })

myToEncoding, myToEncodingCml :: (Generic a, J.GToEncoding J.Zero (Rep a)) => a -> J.Encoding
myToEncoding = J.genericToEncoding (J.defaultOptions { J.omitNothingFields = True, J.fieldLabelModifier = labelMod, J.constructorTagModifier = tagMod })
myToEncodingCml = J.genericToEncoding (J.defaultOptions { J.omitNothingFields = True, J.fieldLabelModifier = labelModCml })

labelMod, tagMod, labelModCml :: String -> String
labelMod    = J.camelTo2 '_' . dropWhile (not . C.isUpper)
tagMod    = J.camelTo2 '_' . dropWhile C.isUpper
labelModCml = uncap        . dropWhile (not . C.isUpper)
  where
    uncap []     = []
    uncap (c:cs) = C.toLower c : cs

type API =       "v2" :> "idxstates" :> "latest" :> Get '[JSON] PkgIdxTs
            :<|> "v2" :> "info"      :> Get '[JSON] ControllerInfo -- static meta-information
            :<|> "v2" :> "packages"  :> Get '[JSON] (Vector PkgN)
            :<|> "v2" :> "packages"  :> "*"                        :> "history" :> QueryParam "min" PkgIdxTs :> QueryParam "max" PkgIdxTs :> Get '[JSON] (Vector IdxHistoryEntry)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN     :> "history" :> Get '[JSON] (Vector PkgHistoryEntry)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN     :> "reports" :> Get '[JSON] (Set PkgIdxTs)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN     :> "reports" :> Capture "idxstate" PkgIdxTs :> Get '[JSON] PkgIdxTsReport
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN     :> "reports" :> Capture "idxstate" PkgIdxTs :> Capture "pkgver" Ver :> Capture "hcver" CompilerID :> Get '[JSON] CellReportDetail
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN     :> "tags"    :> Get '[JSON] (Vector TagN)
            :<|> "v2" :> "queue"     :>                               Get '[JSON] (Vector QEntryRow)
            :<|> "v2" :> "queue"     :> Capture "pkgname" PkgN     :> Get '[JSON] (Vector QEntryRow)
            :<|> "v2" :> "queue"     :> Capture "pkgname" PkgN     :> Capture "idxstate" PkgIdxTs :> ReqBody '[JSON] QEntryUpd :> Put '[JSON] QEntryRow
            :<|> "v2" :> "tags"      :> QueryParam "pkgnames" Bool :> Get '[JSON] (Map TagN (Vector PkgN))
            :<|> "v2" :> "tags"      :> QueryParam "pkgnames" Bool :> Get '[JSON] (Vector TagN)
            :<|> "v2" :> "tags"      :> Capture "tagname" TagN     :> Capture "pkgname" PkgN :> Put '[JSON] NoContent
            :<|> "v2" :> "tags"      :> Capture "tagname" TagN     :> Capture "pkgname" PkgN :> Delete '[JSON] NoContent
            :<|> "v2" :> "units"     :> Capture "unitid" UUID      :> Get '[JSON] UnitIdInfo
            :<|> "v2" :> "users"     :> "name"                     :> Capture "username" UserName :> Get '[JSON] UserPkgs
            :<|> "v2" :> "workers"   :> Get '[JSON] (Vector WorkerRow)
            :<|> "v2" :> "workers"   :> Capture "pkgname" PkgN     :> Get '[JSON] (Vector WorkerRow)

--- The following Type API is heavily inspired by QFPL's reflex-realworld-example: https://github.com/qfpl/reflex-realworld-example
data ClientFuns t m = ClientFuns
  { _getV2IdxStates :: Event t () -> m (Event t (ReqResult () PkgIdxTs))
  , _getV2Info :: Event t () -> m (Event t (ReqResult () ControllerInfo))
  , _getV2Packages :: Event t () -> m (Event t (ReqResult () (Vector PkgN)))
  , _getV2PackagesHistory :: Dynamic t (QParam PkgIdxTs) -> Dynamic t (QParam PkgIdxTs) -> Event t () -> m (Event t (ReqResult () (Vector IdxHistoryEntry)))
  , _getV2PackageHistory :: Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () (Vector PkgHistoryEntry)))
  , _getV2PackageReports :: Dynamic t (Either Text PkgN)  -> Event t () -> m (Event t (ReqResult () (Set PkgIdxTs)))
  , _getV2PackageReportSummary :: Dynamic t (Either Text PkgN) -> Dynamic t (Either Text PkgIdxTs) -> Event t () -> m (Event t (ReqResult () PkgIdxTsReport))
  , _getV2PackageReportDetail :: Dynamic t (Either Text PkgN) -> Dynamic t (Either Text PkgIdxTs) -> Dynamic t (Either Text Ver) -> Dynamic t (Either Text CompilerID) -> Event t () -> m (Event t (ReqResult () CellReportDetail))
  , _getV2PackageTags :: Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () (Vector TagN)))
  , _getV2Queue :: Event t () -> m (Event t (ReqResult () (Vector QEntryRow)))
  , _getV2QueuePkg :: Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () (Vector QEntryRow)))
  , _putV2Queue :: Dynamic t (Either Text PkgN) -> Dynamic t (Either Text PkgIdxTs) -> Dynamic t (Either Text QEntryUpd) -> Event t () -> m (Event t (ReqResult () (QEntryRow)))
  , _getV2TagsWithPackage :: Dynamic t (QParam Bool) -> Event t () -> m (Event t (ReqResult () (Map TagN (Vector PkgN))))
  , _getV2TagsWithoutPackage :: Dynamic t (QParam Bool)  -> Event t () -> m (Event t (ReqResult () (Vector TagN)))
  , _putV2PackageTags :: Dynamic t (Either Text TagN) -> Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () NoContent))
  , _deleteV2PackageTags :: Dynamic t (Either Text TagN) -> Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () NoContent))  
  , _getV2UnitInfo :: Dynamic t (Either Text UUID) -> Event t () -> m (Event t (ReqResult () UnitIdInfo))
  , _getV2User :: Dynamic t (Either Text UserName) -> Event t () -> m (Event t (ReqResult () UserPkgs))
  , _getV2Workers :: Event t () -> m (Event t (ReqResult () (Vector WorkerRow)))
  , _getV2WorkersPkg :: Dynamic t (Either Text PkgN) -> Event t () -> m (Event t (ReqResult () (Vector WorkerRow)))
  }
makeLenses ''ClientFuns

getClient :: forall t m. (SupportsServantReflex t m) 
  => ClientFuns t m 
getClient = mkClientFuns'' burlNew
  where 
    mkClientFuns'' bp = ClientFuns { .. }
      where
        (      _getV2IdxStates 
          :<|> _getV2Info
          :<|> _getV2Packages
          :<|> _getV2PackagesHistory
          :<|> _getV2PackageHistory
          :<|> _getV2PackageReports
          :<|> _getV2PackageReportSummary
          :<|> _getV2PackageReportDetail
          :<|> _getV2PackageTags
          :<|> _getV2Queue
          :<|> _getV2QueuePkg
          :<|> _putV2Queue
          :<|> _getV2TagsWithPackage
          :<|> _getV2TagsWithoutPackage
          :<|> _putV2PackageTags
          :<|> _deleteV2PackageTags
          :<|> _getV2UnitInfo
          :<|> _getV2User
          :<|> _getV2Workers
          :<|> _getV2WorkersPkg ) = (clientWithOpts (Proxy :: Proxy API) Proxy (Proxy :: Proxy ()) (constDyn bp) tweakRequest) :: Client t m API ()

type ValidationErrors = Map Text [Text]
type ClientRes t a = (Event t a, Event t ClientError, Dynamic t Bool)
  
data ClientError
  =   Forbidden
  | NotFound
  | Unauthorised
  | FailedValidation (Maybe (ErrorBody ValidationErrors))
  | OtherError Word Text
  deriving (Show)
  
data ErrorBody errors = ErrorBody
  { message :: Text
  , errors  :: Maybe errors
  } deriving (Generic, Show)
        
deriving instance ToJSON errors   => ToJSON (ErrorBody errors)
deriving instance FromJSON errors => FromJSON (ErrorBody errors)

wireClientRes
  :: (Reflex t, MonadHold t m)
  => Event t b
  -> Event t (ReqResult () a)
  -> m (ClientRes t a)
wireClientRes evSubmit resE = do
  let evSuccess = fmapMaybe reqSuccess resE
  let evError   = fmapMaybe reqClientError resE
  submittingDyn <- holdDyn False $ leftmost [True <$ evSubmit, False <$ evError, False <$ evSuccess]
  pure (evSuccess, evError, submittingDyn)

reqClientError :: ReqResult tag a -> Maybe ClientError
reqClientError (ResponseFailure _ msg xhrR) = Just $ case view xhrResponse_status xhrR of
  401 -> Unauthorised
  403 -> Forbidden
  404 -> NotFound
  422 -> FailedValidation (xhrR ^? xhrResponse_responseText . _Just . to fromStrict . to encodeUtf8 . to decode . _Just)
  w   -> OtherError w msg
reqClientError _                              = Nothing

fill :: a -> Getting f (a -> b) b
fill a = to ($ a)

getIdxStates :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) => Event t () -> m (ClientRes t (PkgIdxTs))
getIdxStates evSubmit = do
  evResult <- getClient ^. getV2IdxStates . fill evSubmit
  wireClientRes evSubmit evResult

getInfo :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) => Event t () -> m (ClientRes t (ControllerInfo))
getInfo evSubmit = do
  evResult <- getClient ^. getV2Info . fill evSubmit
  wireClientRes evSubmit evResult

getPackages :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) => Event t () -> m (ClientRes t (Vector PkgN))
getPackages evSubmit = do
  evResult <- getClient ^. getV2Packages . fill evSubmit
  wireClientRes evSubmit evResult

getPackagesHistory :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) 
                   => Dynamic t (QParam PkgIdxTs) 
                   -> Dynamic t (QParam PkgIdxTs) 
                   -> Event t () 
                   -> m (ClientRes t (Vector IdxHistoryEntry))
getPackagesHistory minDyn maxDyn evSubmit = do
  evResult <- getClient ^. getV2PackagesHistory . fill minDyn . fill maxDyn . fill evSubmit
  wireClientRes evSubmit evResult

getPackageHistory :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
                  => Dynamic t (Either Text PkgN)
                  -> Event t ()
                  -> m (ClientRes t (Vector PkgHistoryEntry))
getPackageHistory pkgNDyn evSubmit = do
  evResult <- getClient ^. getV2PackageHistory . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult

getPackageReports :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
                  => Dynamic t (Either Text PkgN) 
                  -> Event t () 
                  -> m (ClientRes t (Set PkgIdxTs))
getPackageReports pkgNDyn evSubmit = do
  evResult <- getClient ^. getV2PackageReports . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult
       
getPackageReportSummary :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
                        => Dynamic t (Either Text PkgN)
                        -> Dynamic t (Either Text PkgIdxTs)
                        -> Event t ()
                        -> m (ClientRes t PkgIdxTsReport)
getPackageReportSummary pkgNDyn pkgIdxDyn evSubmit = do
  evResult <- getClient ^. getV2PackageReportSummary . fill pkgNDyn . fill pkgIdxDyn . fill evSubmit
  wireClientRes evSubmit evResult

getPackageReportDetail :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
                       => Dynamic t (Either Text PkgN)
                       -> Dynamic t (Either Text PkgIdxTs)
                       -> Dynamic t (Either Text Ver)
                       -> Dynamic t (Either Text CompilerID)
                       -> Event t ()
                       -> m (ClientRes t CellReportDetail)
getPackageReportDetail pkgNDyn pkgIdxDyn verDyn compilerDyn evSubmit = do
  evResult <- getClient ^. getV2PackageReportDetail . fill pkgNDyn . fill pkgIdxDyn . fill verDyn . fill compilerDyn . fill evSubmit
  wireClientRes evSubmit evResult

getPackageTags :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
               => Dynamic t (Either Text PkgN) 
               -> Event t () 
               -> m (ClientRes t (Vector TagN))
getPackageTags pkgNDyn evSubmit = do
  evResult <- getClient ^. getV2PackageTags . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult 

getQueue :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
         => Event t ()
         -> m (ClientRes t (Vector QEntryRow))
getQueue evSubmit = do
  evResult <- getClient ^. getV2Queue . fill evSubmit
  wireClientRes evSubmit evResult

getQueuePkg :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) 
            => Dynamic t (Either Text PkgN)
            -> Event t ()
            -> m (ClientRes t (Vector QEntryRow))
getQueuePkg pkgNDyn evSubmit = do
  evResult <- getClient ^. getV2QueuePkg . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult 

putQueue :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) 
         => Dynamic t (Either Text PkgN)
         -> Dynamic t (Either Text PkgIdxTs)
         -> Dynamic t (Either Text QEntryUpd)
         -> Event t ()
         -> m (ClientRes t (QEntryRow))
putQueue pkgNDyn pkgIdxDyn qEntryDyn evSubmit = do
  evResult <- getClient ^. putV2Queue . fill pkgNDyn . fill pkgIdxDyn . fill qEntryDyn . fill evSubmit
  wireClientRes evSubmit evResult

getTagsPkg :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) 
           => Dynamic t (QParam Bool) 
           -> Event t () 
           -> m (ClientRes t (Map TagN (Vector PkgN)))
getTagsPkg dynBool evSubmit = do
  evResult <- getClient ^. getV2TagsWithPackage . fill dynBool . fill evSubmit
  wireClientRes evSubmit evResult

getTags :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
        => Dynamic t (QParam Bool) 
        -> Event t () 
        -> m (ClientRes t (Vector TagN))
getTags dynBool evSubmit = do
  evResult <- getClient ^. getV2TagsWithoutPackage . fill dynBool . fill evSubmit
  wireClientRes evSubmit evResult

putTags :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
        => Dynamic t (Either Text TagN) 
        -> Dynamic t (Either Text PkgN) 
        -> Event t () 
        -> m (ClientRes t NoContent)
putTags tagNDyn pkgNDyn evSubmit = do
  evResult <- getClient ^. putV2PackageTags . fill tagNDyn . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult

deleteTags :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
           => Dynamic t (Either Text TagN) 
           -> Dynamic t (Either Text PkgN) 
           -> Event t () 
           -> m (ClientRes t NoContent)
deleteTags tagNDyn pkgNDyn evSubmit = do
  evResult <- getClient ^. deleteV2PackageTags . fill tagNDyn . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult 

getUnitInfo :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
            => Dynamic t (Either Text UUID) 
            -> Event t () 
            -> m (ClientRes t (UnitIdInfo))
getUnitInfo uuidDyn evSubmit = do
  evResult <- getClient ^. getV2UnitInfo . fill uuidDyn . fill evSubmit
  wireClientRes evSubmit evResult

getUser :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
        => Dynamic t (Either Text UserName)
        -> Event t ()
        -> m (ClientRes t UserPkgs)
getUser usrNDyn evSubmit = do
  evResult <- getClient ^. getV2User . fill usrNDyn . fill evSubmit
  wireClientRes evSubmit evResult

getWorkers :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m)
           => Event t ()
           -> m (ClientRes t (Vector WorkerRow))
getWorkers evSubmit = do
  evResult <- getClient ^. getV2Workers . fill evSubmit
  wireClientRes evSubmit evResult 

getWorkersPkg :: forall t m. (Reflex t, SupportsServantReflex t m, MonadHold t m) 
              => Dynamic t (Either Text PkgN)
              -> Event t ()
              -> m (ClientRes t (Vector WorkerRow))
getWorkersPkg pkgNDyn evSubmit = do
  evResult <- getClient ^. getV2WorkersPkg . fill pkgNDyn . fill evSubmit
  wireClientRes evSubmit evResult 
----------------------------------------------------------------------------
burlNew :: BaseUrl
burlNew | True      = BaseFullUrl Https "matrix.hackage.haskell.org" 443 "/api"
        | otherwise = BasePath "/api"

tweakRequest :: ClientOptions
tweakRequest = ClientOptions $ \r -> do
  return $ r & withCredentials .~ True