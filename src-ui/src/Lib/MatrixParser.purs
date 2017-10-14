module Lib.MatrixParser where

import Prelude (pure, show, ($), (<$>), (<<<))
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff.Class (class MonadAff)
import Data.Traversable as TRV
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core as Arg
import Data.Either (Either(..))
import Network.RemoteData as RD
import Control.Monad.Eff.Exception as E
import Data.Int as Int
import Data.Array
import Data.Tuple.Nested as TupleN
import Data.StrMap as SM
import Lib.MiscFFI

import Lib.Types as T

toPackageIdxTsReports :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                      => Either String Arg.Json
                      -> m (RD.RemoteData E.Error T.PackageIdxTsReports)
toPackageIdxTsReports (Right a) =
  case Arg.toObject a of
    Just obj ->
      let
        pkgName =
          case SM.lookup "pkgname" obj of
            Just pkgN ->
              case Arg.toString pkgN of
                Just pn -> pn
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        idxState =
          case SM.lookup "idxstate" obj of
            Just idx ->
              case Arg.toNumber idx of
                Just idxts -> Int.round idxts
                Nothing -> 0
            Nothing -> 0
        hcVersions =
           case SM.lookup "hcversions" obj of
            Just arr ->
              case Arg.toArray arr of
                Just arrHc -> toString <$> arrHc
                Nothing -> []
            Nothing -> []
        pkgVersions =
          case SM.lookup "pkgversions" obj of
            Just pkgObj ->
              case Arg.toObject pkgObj of
                Just ob -> toCellReportSummary <$> ob
                Nothing -> SM.empty
            Nothing -> SM.empty
      in pure $ RD.Success
                  {
                    pkgname : pkgName
                  , idxstate : idxState
                  , hcversions : hcVersions
                  , pkgversions : pkgVersions
                  }

    Nothing -> pure (RD.Failure (E.error "Json is not an object"))
toPackageIdxTsReports (Left e) = pure (RD.Failure (E.error e))

toCellReportSummary :: Arg.Json -> Array T.CellReportSummary
toCellReportSummary json =
  case Arg.toArray json of
    Just arr -> cellReport <$> arr
    Nothing -> []

cellReport :: Arg.Json -> T.CellReportSummary
cellReport jobj=
  case Arg.toObject jobj of
    Just obj ->
      let
        reporttype =
          case SM.lookup "t" obj of
            Just typeR ->
              case Arg.toString typeR of
                Just tr -> tr
                Nothing -> "type is not String"
            Nothing -> "type does not exist"
        bjle =
          case SM.lookup "bjle" obj of
            Just buildJle ->
              case Arg.toNumber buildJle of
                Just bj -> Int.round bj
                Nothing -> 0
            Nothing -> 0
        perr =
          case SM.lookup "perr" obj of
            Just planErr ->
              case Arg.toBoolean planErr of
                Just a -> true
                Nothing -> false
            Nothing -> false
        bok =
          case SM.lookup "bok" obj of
            Just buildOk ->
              case Arg.toNumber buildOk of
                Just bo -> Int.round bo
                Nothing -> 0
            Nothing -> 0
        bfail =
          case SM.lookup "bfail" obj of
            Just buildFail ->
              case Arg.toNumber buildFail of
                Just bf -> Int.round bf
                Nothing -> 0
            Nothing -> 0
        bdfail =
          case SM.lookup "bdfail" obj of
            Just buildDFail ->
              case Arg.toNumber buildDFail of
                Just bd -> Int.round bd
                Nothing -> 0
            Nothing -> 0
      in
       {
         crsT : reporttype
       , crsBjle : bjle
       , crsPerr : perr
       , crsBok : bok
       , crsBfail : bfail
       , crsBdfail : bdfail
       }
    Nothing ->
      {
        crsT : "na"
      , crsBjle : 0
      , crsPerr : false
      , crsBok : 0
      , crsBfail : 0
      , crsBdfail : 0
      }

toCellReportDetail :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                   => Either String Arg.Json
                   -> m (RD.RemoteData E.Error T.CellReportDetail)
toCellReportDetail (Right a) =
   case Arg.toObject a of
    Just obj ->
      let
        pkgName =
          case SM.lookup "pkgname" obj of
            Just pkgN ->
              case Arg.toString pkgN of
                Just pn -> pn
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        pkgVersion =
           case SM.lookup "pkgversion" obj of
            Just pkgver ->
              case Arg.toString pkgver of
                Just pv -> pv
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        idxState =
          case SM.lookup "idxstate" obj of
            Just idx ->
              case Arg.toNumber idx of
                Just idxts -> Int.round idxts
                Nothing -> 0
            Nothing -> 0
        hcVersion =
           case SM.lookup "hcversion" obj of
            Just hcver ->
              case Arg.toString hcver of
                Just hc -> hc
                Nothing -> "hcversion is not String"
            Nothing -> "hcversion does not exist"
        reportType =
          case SM.lookup "type" obj of
            Just reportT ->
              case Arg.toString reportT of
                Just rt -> rt
                Nothing -> "report type is not String"
            Nothing -> "report type does not exist"
        solverError =
          case SM.lookup "solver_err" obj of
            Just solverE ->
              case Arg.toString solverE of
                Just se -> se
                Nothing -> "solver is not String"
            Nothing -> "na"
        unitsHistory =
          case SM.lookup "units" obj of
            Just unitsH ->
              case Arg.toArray unitsH of
                Just units -> toArrayMap <$> units
                Nothing -> []
            Nothing -> []
      in pure $ RD.Success
                  {
                    pkgname : pkgName
                  , pkgversion : pkgVersion
                  , idxstate : idxState
                  , hcversion : hcVersion
                  , reporttype : reportType
                  , solverE : solverError
                  , units : unitsHistory
                  }

    Nothing -> pure (RD.Failure (E.error "Json is not an object"))
toCellReportDetail (Left e) = pure (RD.Failure (E.error e))

toPackageHistories :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                   => Either String Arg.Json
                   -> m (RD.RemoteData E.Error T.PackageHistories)
toPackageHistories (Right a) =
  case Arg.toArray a of
    Just arr -> pure (RD.Success (toTuple4 <$> arr))
    Nothing -> pure (RD.Failure (E.error "json is not array"))
toPackageHistories (Left e) = pure (RD.Failure (E.error e))

toTuple4 :: Arg.Json -> T.PackageHistory
toTuple4 json =
  case Arg.toArray json of
    Just arr ->
      let
        pkgIdx =
          case index arr 0 of
            Just idx ->
              case Arg.toNumber idx of
                Just pi -> Int.round pi
                Nothing -> 0
            Nothing -> 0
        verName =
          case index arr 1 of
            Just vname ->
              case Arg.toString vname of
                Just a -> a
                Nothing -> "na"
            Nothing -> "na"
        revision =
           case index arr 2 of
            Just rev ->
              case Arg.toNumber rev of
                Just r -> Int.round r
                Nothing -> 0
            Nothing -> 0
        userName =
          case index arr 3 of
            Just uname ->
              case Arg.toString uname of
                Just a -> a
                Nothing -> "na"
            Nothing -> "na"
      in
       TupleN.tuple4 pkgIdx verName revision userName
    Nothing -> TupleN.tuple4 0 "na" 0 "na"

toUnitIdInfo :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
             => Either String Arg.Json
             -> m (RD.RemoteData E.Error T.UnitIdInfo)
toUnitIdInfo (Right a) =
  case Arg.toObject a of
    Just obj ->
      let
        unitId =
          case SM.lookup "id" obj of
            Just uid ->
              case Arg.toString uid of
                Just ui -> ui
                Nothing -> "unit id is not String"
            Nothing -> "unit id does not exist"
        hcVersion =
          case SM.lookup "hcver" obj of
            Just hcver ->
              case Arg.toString hcver of
                Just hc -> hc
                Nothing -> "hcversion is not String"
            Nothing -> "hcversion does not exist"
        pkgName =
          case SM.lookup "pkgname" obj of
            Just pkgN ->
              case Arg.toString pkgN of
                Just pn -> pn
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        pkgVersion =
           case SM.lookup "pkgver" obj of
            Just pkgVer ->
              case Arg.toString pkgVer of
                Just pv -> pv
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        status =
           case SM.lookup "status" obj of
            Just stat ->
              case Arg.toString stat of
                Just st -> st
                Nothing -> "status is not String"
            Nothing -> "status name does not exist"
        logMsg =
           case SM.lookup "logmsg" obj of
            Just logm ->
              case Arg.toString logm of
                Just lm -> lm
                Nothing -> "log message is not String"
            Nothing -> "log message name does not exist"
        libDeps =
          case SM.lookup "lib_deps" obj of
            Just libD ->
              case Arg.toObject libD of
                Just ld -> toArrayString <$> ld
                Nothing -> SM.empty
            Nothing -> SM.empty
        exeDeps =
          case SM.lookup "exe_deps" obj of
            Just exeD ->
              case Arg.toObject exeD of
                Just ed -> toArrayString <$> ed
                Nothing -> SM.empty
            Nothing -> SM.empty
      in
       pure $ RD.Success
                {
                  uiiId : unitId
                , uiiHcVer : hcVersion
                , uiiPkgname : pkgName
                , uiiPkgver : pkgVersion
                , uiiStatus : status
                , uiiLogmsg : logMsg
                , uiiLibDeps : libDeps
                , uiiExeDeps : exeDeps
                }
    Nothing -> pure (RD.Failure (E.error "Json is not object"))
toUnitIdInfo (Left e) = pure (RD.Failure (E.error e))

toUnitIdInfoDeps :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                 => Either String Arg.Json
                 -> m (RD.RemoteData E.Error T.UnitIdInfoDeps)
toUnitIdInfoDeps (Right a) =
  case Arg.toObject a of
    Just obj -> pure (RD.Success (toUnitIdInfoDep <$> obj))
    Nothing  -> pure (RD.Failure (E.error "Json is not object"))
toUnitIdInfoDeps (Left e) = pure (RD.Failure (E.error e))

toUnitIdInfoDep :: Arg.Json -> T.UnitIdInfoDep
toUnitIdInfoDep a =
  case Arg.toObject a of
    Just obj ->
      let
        pkgName =
          case SM.lookup "pkgname" obj of
            Just pkgN ->
              case Arg.toString pkgN of
                Just pn -> pn
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        pkgVersion =
           case SM.lookup "pkgver" obj of
            Just pkgVer ->
              case Arg.toString pkgVer of
                Just pv -> pv
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        status =
           case SM.lookup "status" obj of
            Just stat ->
              case Arg.toString stat of
                Just st -> st
                Nothing -> "status is not String"
            Nothing -> "status name does not exist"
        libDeps =
          case SM.lookup "lib_deps" obj of
            Just libD ->
              case Arg.toObject libD of
                Just ld -> toArrayString <$> ld
                Nothing -> SM.empty
            Nothing -> SM.empty
        exeDeps =
          case SM.lookup "exe_deps" obj of
            Just exeD ->
              case Arg.toObject exeD of
                Just ed -> toArrayString <$> ed
                Nothing -> SM.empty
            Nothing -> SM.empty
      in
        { pkgname : pkgName
        , pkgver : pkgVersion
        , status : status
        , lib_deps : libDeps
        , exe_deps : exeDeps
        }
    Nothing -> T.unitIdInfoDepEmpty

toUser ::  forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
       => Either String Arg.Json
       -> m (RD.RemoteData E.Error T.User)
toUser (Right json) =
  case Arg.toObject json of
    Just obj ->
      let
        usrName =
          case SM.lookup "name" obj of
            Just pkgN ->
              case Arg.toString pkgN of
                Just pn -> pn
                Nothing -> "user is not String"
            Nothing -> "user name does not exist"
        usrPackages =
           case SM.lookup "packages" obj of
            Just pkgVer ->
              case Arg.toArray pkgVer of
                Just pv -> toString <$> pv
                Nothing -> []
            Nothing -> []
      in pure $ RD.Success { name : usrName, packages : usrPackages}
    Nothing -> pure $ RD.Failure (E.error "failed to parse User")
toUser (Left e) = pure $ RD.Failure (E.error "failed to parse User")

toString :: Arg.Json -> String
toString json =
  case Arg.toString json of
    Just arr -> arr
    Nothing -> "na"

toArrayString :: Arg.Json -> Array String
toArrayString json =
  case Arg.toArray json of
    Just arr -> toString <$> arr
    Nothing -> []

toArrayMap :: Arg.Json -> SM.StrMap String
toArrayMap json =
  case Arg.toObject json of
    Just obj -> toString <$> obj
    Nothing  -> SM.empty

toPackageQueue  :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                => Either String Arg.Json
                -> m (RD.RemoteData E.Error (Array T.PackageQueue))
toPackageQueue (Right a) =
  case Arg.toArray a of
    Just arr -> pure (RD.Success (toObjectQueue <$> arr))
    Nothing -> pure (RD.Failure (E.error "Json is not Array"))
toPackageQueue (Left e) = pure (RD.Failure (E.error e))

toSpecificPackageQueue  :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
                => Either String Arg.Json
                -> m (RD.RemoteData E.Error T.PackageQueue)
toSpecificPackageQueue (Right a) = pure (RD.Success (toObjectQueue a))
toSpecificPackageQueue (Left e) = pure (RD.Failure (E.error e))

toObjectQueue :: Arg.Json -> T.PackageQueue
toObjectQueue json =
  case Arg.toObject json of
    Just obj ->
      let
        prio =
          case SM.lookup "priority" obj of
            Just pr ->
              case Arg.toNumber pr of
                Just p -> Int.round p
                Nothing -> 0
            Nothing -> 0
        mod =
          case SM.lookup "modified" obj of
            Just pkgName ->
              case Arg.toString pkgName of
                Just pkgName' -> pkgName'
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"

        pkg =
          case SM.lookup "pkgname" obj of
            Just pkgName ->
              case Arg.toString pkgName of
                Just pkgName' -> pkgName'
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        idx =
          case SM.lookup "idxstate" obj of
            Just idx ->
              case Arg.toNumber idx of
                Just idxts -> Int.round idxts
                Nothing -> 0
            Nothing -> 0
      in
       {
         priority : prio
       , modified : mod
       , pkgname : pkg
       , idxstate : idx
       }
    Nothing ->
      {
        priority : 0
      , modified : ""
      , pkgname : ""
      , idxstate : 0
      }
