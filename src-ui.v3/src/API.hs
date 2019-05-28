{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
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

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module API
    ( ClientFuns(..)
    , mkClientFuns

    , PkgIdxTs
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
    ) where

import           PkgId

import           Control.Monad    (fail)
import           Data.Aeson       (FromJSON)
import qualified Data.Aeson       as J
import qualified Data.Aeson.Types as J
import qualified Data.Char        as C
import           Data.Map         (Map)
import           Data.Proxy
import           Data.Set         (Set)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time        (UTCTime)
import           Data.UUID.Types  (UUID)
import           Data.Vector      (Vector)
import           GHC.Generics     (Rep)
import           Reflex.Dom hiding (Client)
import           Servant.API
import           Servant.Reflex

data ClientFuns t m = ClientFuns
     { getV2IdxStatesLatest      :: Client t m (Get '[JSON] PkgIdxTs) ()
     , getV2Queue                :: Client t m (Get '[JSON] (Vector QEntryRow)) ()
     , getV2QueuePkg             :: Client t m (Capture "" PkgN :> Get '[JSON] (Vector QEntryRow)) ()
     , putV2Queue                :: Client t m (Capture "" PkgN :> Capture "" PkgIdxTs :> ReqBody '[JSON] QEntryUpd :> Put '[JSON] QEntryRow) ()
     , getV2Packages             :: Client t m (Get '[JSON] (Vector PkgN)) ()
     , getV2PackagesHistory      :: Client t m (QueryParam "min" PkgIdxTs :> QueryParam "max" PkgIdxTs :> Get '[JSON] (Vector IdxHistoryEntry)) ()
     , getV2PackageHistory       :: Client t m (Capture "" PkgN :> Get '[JSON] (Vector PkgHistoryEntry)) ()
     , getV2PackageReports       :: Client t m (Capture "" PkgN :> Get '[JSON] (Set PkgIdxTs)) ()
     , getV2PackageReportSummary :: Client t m (Capture "" PkgN :> Capture "" PkgIdxTs :> Get '[JSON] PkgIdxTsReport) ()
     , getV2PackageReportDetail  :: Client t m (Capture "" PkgN :> Capture "" PkgIdxTs :> Capture "" Ver :> Capture "" CompilerID :> Get '[JSON] CellReportDetail) ()

     , getV2UnitInfo             :: Client t m (Capture "" UUID :> Get '[JSON] UnitIdInfo) ()

     , getV2Workers              :: Client t m (Get '[JSON] (Vector WorkerRow)) ()
     , getV2WorkersPkg           :: Client t m (Capture "" PkgN :> Get '[JSON] (Vector WorkerRow)) ()
     , getV2User                 :: Client t m (Capture "" UserName :> Get '[JSON] UserPkgs) ()
     , getV2Info                 :: Client t m (Get '[JSON] ControllerInfo) ()
     }

mkClientFuns :: forall t m . (HasClient t m API (), Reflex t) => BaseUrl -> ClientFuns t m
mkClientFuns burl = ClientFuns {..}
  where
    (     getV2Info
     :<|> getV2IdxStatesLatest
     :<|> getV2Queue
     :<|> getV2QueuePkg
     :<|> putV2Queue
     :<|> getV2Packages
     :<|> getV2PackagesHistory
     :<|> getV2PackageHistory
     :<|> getV2PackageReports
     :<|> getV2PackageReportSummary
     :<|> getV2PackageReportDetail
     :<|> getV2UnitInfo
     :<|> getV2Workers
     :<|> getV2WorkersPkg
     :<|> getV2User
     ) = (client (Proxy :: Proxy API) Proxy (Proxy :: Proxy ()) (constDyn burl)) :: Client t m API ()

-- subset taken from "Controller.Api"
type API =       "v2" :> "info"      :> Get '[JSON] ControllerInfo -- static meta-information
            :<|> "v2" :> "idxstates" :> "latest" :> Get '[JSON] PkgIdxTs
            :<|> "v2" :> "queue"     :>                           Get '[JSON] (Vector QEntryRow)
            :<|> "v2" :> "queue"     :> Capture "pkgname" PkgN :> Get '[JSON] (Vector QEntryRow)
            :<|> "v2" :> "queue"     :> Capture "pkgname" PkgN :> Capture "idxstate" PkgIdxTs :> ReqBody '[JSON] QEntryUpd :> Put '[JSON] QEntryRow
            :<|> "v2" :> "packages"  :> Get '[JSON] (Vector PkgN)
            :<|> "v2" :> "packages"  :> "*"                    :> "history" :> QueryParam "min" PkgIdxTs :> QueryParam "max" PkgIdxTs :> Get '[JSON] (Vector IdxHistoryEntry)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN :> "history" :> Get '[JSON] (Vector PkgHistoryEntry)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN :> "reports" :> Get '[JSON] (Set PkgIdxTs)
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN :> "reports" :> Capture "idxstate" PkgIdxTs :> Get '[JSON] PkgIdxTsReport
            :<|> "v2" :> "packages"  :> Capture "pkgname" PkgN :> "reports" :> Capture "idxstate" PkgIdxTs :> Capture "pkgver" Ver :> Capture "hcver" CompilerID :> Get '[JSON] CellReportDetail

            :<|> "v2" :> "units" :> Capture "unitid" UUID :> Get '[JSON] UnitIdInfo

            :<|> "v2" :> "workers"   :> Get '[JSON] (Vector WorkerRow)
            :<|> "v2" :> "workers"   :> Capture "pkgname" PkgN :> Get '[JSON] (Vector WorkerRow)
            :<|> "v2" :> "users" :> "name" :> Capture "username" UserName :> Get '[JSON] UserPkgs

----------------------------------------------------------------------------

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

