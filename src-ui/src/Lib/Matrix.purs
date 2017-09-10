module Lib.MatrixApi where

import Control.Monad.Eff
import Control.Monad.Eff.Exception as E
import Data.Function.Uncurried as U
import Lib.Types as T
import Network.RemoteData as RD
import Run as R
import Control.Monad.Aff (Aff, attempt, makeAff, launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Reader (class MonadReader, asks, ReaderT)
import Control.Monad.Except as Except
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Halogen.Aff (HalogenEffects)
import Lib.MiscFFI as Misc
import Lib.Undefined (Undefined)
import Lib.Uri (Uri)
import Prelude (Unit, pure, bind, const, map, ($), (<<<), (<>), (<$>), show, (/=), (==), otherwise)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response as Affjax
import Data.HTTP.Method (Method(..))
import Data.Either (Either(..))
import Data.String as Str
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexF
import Data.Array as Arr
import Data.Traversable as TRV
import Data.Maybe as M
import Data.StrMap as SM
import Network.HTTP.StatusCode as SC
import Data.Argonaut as Arg
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode as Arg
import DOM
import DOM.HTML.Types as DOM
import Data.Tuple as Tuple
import Data.Int as Int

foreign import data MatrixApi :: Type


foreign import data API :: Effect

type Environment e = { matrixClient :: MatrixApi
                   , packageList :: Ref (RD.RemoteData (Eff (api :: API | e) Unit) (T.ApiList T.PackageMeta))
                   }

type MatrixApi' eff = ReaderT (Environment eff) (Aff (HalogenEffects (api :: API, ajax :: Affjax.AJAX  , dom :: DOM, history :: DOM.HISTORY| eff)))

type Matrix eff = MatrixApi' eff

type MatrixEffects = HalogenEffects (api :: API, ajax :: Affjax.AJAX, dom :: DOM, history :: DOM.HISTORY)

foreign import newApi :: forall eff
   . String
  -> String
  -> Eff (api :: API | eff) MatrixApi

foreign import data JQueryXHR :: Type

type ApiEff e o = Eff (api :: API | e) o

getTimestamp :: forall e m a b. MonadAff (ajax :: Affjax.AJAX | e) m
             => T.PackageName
             -> m (RD.RemoteData String Arg.Json)
getTimestamp pkgName = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/packages/" <> pkgName <> "/reports"
                                 , method = Left GET
                                 })
  let
    decodedApi = Arg.decodeJson (res.response :: Arg.Json)
  pure (RD.fromEither decodedApi)

getLatestReportByPackageTimestamp :: forall e m a b. MonadAff (ajax :: Affjax.AJAX | e) m
                                  => T.PackageName
                                  -> T.PackageTS
                                  -> m (RD.RemoteData E.Error Arg.Json)
getLatestReportByPackageTimestamp pkgName pkgTs = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v1.0.0/package/name/" <> pkgName <> "/report/idxstate/"<> pkgTs
                                 , method = Left GET
                                 })
  let
    decodedApi = Arg.decodeJson (res.response :: Arg.Json)
  case decodedApi of
    (Right a) -> pure (RD.Success a)
    (Left e)  -> pure (RD.Failure (E.error e))

parseShallowReport :: RD.RemoteData E.Error Arg.Json
                 -> RD.RemoteData E.Error T.ShallowReport
parseShallowReport (RD.Success a) =
  case Arg.toObject a of
    Just a' ->
      let
        packageName' =
          case SM.lookup "packageName" a' of
            Just pkgName ->
              case Arg.toString pkgName of
                Just pkgName' -> pkgName'
                Nothing -> "package is not String"
            Nothing -> "package name does not exist"
        modified' =
          case SM.lookup "modified" a' of
            Just mod ->
              case Arg.toString mod of
                Just mod' -> mod'
                Nothing -> "modified is not string"
            Nothing -> "modified does not exist"
        results' =
          case SM.lookup "results" a' of
            Just res ->
              case Arg.toArray res of
                Just res' -> parseShallowGhcResult (TRV.traverse Arg.toObject res')
                Nothing -> []
            Nothing -> []
      in RD.Success
           { packageName : packageName'
           , modified : modified'
           , results : results'
           }
    Nothing -> RD.Failure (E.error "This is not an Object")
parseShallowReport (RD.Failure e) = RD.Failure e
parseShallowReport RD.Loading = RD.Loading
parseShallowReport RD.NotAsked = RD.NotAsked

shallowReportDefault :: T.ShallowReport
shallowReportDefault =
  { packageName: ""
  , modified: ""
  , results: []
  }

parseShallowGhcResult :: Maybe (Array Arg.JObject) -> Array T.ShallowGhcResult
parseShallowGhcResult (Just a) = singleShallowGhcResult <$> a
parseShallowGhcResult Nothing = []

singleShallowGhcResult :: Arg.JObject -> T.ShallowGhcResult
singleShallowGhcResult jObj =
  let
    ghcVer' =
      case SM.lookup "ghcVersion" jObj of
        Just ver ->
          case Arg.toString ver of
            Just ver' -> ver'
            Nothing -> "version is not String"
        Nothing -> "version does not exist"
    ghcFullVer' =
      case SM.lookup "ghcFullVersion" jObj of
        Just fullVer ->
          case Arg.toString fullVer of
            Just fullVer' -> fullVer'
            Nothing -> "full version is not String"
        Nothing -> "full version does not exist"
    ghcResult' =
      case SM.lookup "ghcResult" jObj of
        Just result ->
          case Arg.toArray result of
            Just result' -> parseShallowVersionResult (TRV.traverse Arg.toObject result')
            Nothing -> []
        Nothing -> []
  in
   { ghcVersion : ghcVer'
   , ghcFullVersion : ghcFullVer'
   , ghcResult : ghcResult'
   }

shallowGhcResultDefault :: T.ShallowGhcResult
shallowGhcResultDefault =
  { ghcVersion     : ""
  , ghcFullVersion : ""
  , ghcResult      : []
  }

parseShallowVersionResult :: Maybe (Array Arg.JObject) -> Array T.ShallowVersionResult
parseShallowVersionResult (Just a) = singleShallowVersionResult <$> a
parseShallowVersionResult Nothing = []

singleShallowVersionResult :: Arg.JObject -> T.ShallowVersionResult
singleShallowVersionResult jObj =
  let
    pkgVer' =
      case SM.lookup "packageVersion" jObj of
        Just ver ->
          case Arg.toString ver of
            Just ver' -> ver'
            Nothing -> "version is not String"
        Nothing -> "version does not exist"
    pkgRev' =
      case SM.lookup "packageRevision" jObj of
        Just rev ->
          case Arg.toNumber rev of
            Just rev' -> Int.round rev'
            Nothing -> 0
        Nothing -> 0
    result' =
      case SM.lookup "result" jObj of
        Just result ->
          case Arg.toObject result of
            Just result' -> parseShallowResult result'
            Nothing -> T.Unknown
        Nothing -> T.Unknown
  in
   { packageVersion : pkgVer'
   , packageRevision : pkgRev'
   , result : result'
   }

parseShallowResult :: Arg.JObject -> T.ShallowResult
parseShallowResult jObj =
  case Arr.head (SM.keys jObj) of
    Just a -> toShallowResult a
    Nothing -> T.Unknown

toShallowResult :: String -> T.ShallowResult
toShallowResult  str
  | str == "ok" = T.ShallowOk
  | str == "nop" = T.ShallowNop
  | str == "noip" = T.ShallowNoIp
  | str == "noipbjlimit" = T.ShallowNoIpBjLimit 0
  | str == "noipfail" = T.ShallowNoIpFail
  | str == "fail" = T.ShallowFail
  | str == "faildeps" = T.ShallowFailDeps 0
  | otherwise = T.Unknown



shallowVersionResultDefault :: T.ShallowVersionResult
shallowVersionResultDefault =
 { packageVersion  : ""
 , packageRevision : 0
 , result          : T.Unknown
 }

latestIndex :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
            => T.PackageName
            -> m (Tuple.Tuple T.PackageName String)
latestIndex pkgName = do
  idx <- getTimestamp pkgName
  case idx of
    RD.Success idx -> getIdx ((show <<< Int.round) <$> (Misc.fromIndexToNumber (Arg.toArray idx))) pkgName
    _              -> pure $ Tuple.Tuple "" ""
  where
    getIdx idx' pkg'=
      case Arr.last idx' of
        Just a  -> pure $ Tuple.Tuple pkg' a
        Nothing -> pure $ Tuple.Tuple "" ""

getPackageList :: forall e a m. MonadReader (Environment a) m
               => MonadAff (api :: API | e) m
               => m (T.ApiList T.PackageMeta)
getPackageList = do
  client <- asks _.matrixClient
  liftAff (packageList client { count : (Just 100000), offset : Nothing })

getQueueByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
             => MonadAff (api :: API | e) m
             => T.PackageName
             -> m (RD.RemoteData E.Error (Maybe T.QueueItem))
getQueueByName pkg = do
  client <- asks _.matrixClient
  queueI <- liftAff (attempt (queueByName client pkg))
  pure (RD.fromEither queueI)

getQueueList :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
             => MonadAff (api :: API | e) m
             => m (T.ApiList T.QueueItem)
getQueueList = do
  client <- asks _.matrixClient
  liftAff (queueList client)


getTagList :: forall a e m. MonadReader { matrixClient :: MatrixApi | a} m
           => MonadAff (api :: API | e) m
           => m (T.ApiList T.Tag)
getTagList = do
  client <- asks _.matrixClient
  liftAff (tagList client)

getListLatestReports :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                     => MonadAff (api :: API | e) m
                     => m (T.ApiList T.LatestItem)
getListLatestReports = do
  client <- asks _.matrixClient
  liftAff (listLatestReports client { count : (Just 50), offset : Nothing })

putQueueSaveByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                   => MonadAff (api :: API | e) m
                   => T.PackageName
                   -> T.Priority
                   -> m Unit
putQueueSaveByName pkgName priority = do
  client <- asks _.matrixClient
  liftAff (queueSaveByName client pkgName priority)

putQueueCreate :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
               => MonadAff (api :: API | e) m
               => T.PackageName
               -> T.Priority
               -> m Unit
putQueueCreate pkgName priority = do
  client <- asks _.matrixClient
  liftAff (queueCreate client pkgName priority)


deleteQueueRemove :: forall a e m.
                     MonadReader { matrixClient :: MatrixApi | a } m
                  => MonadAff (api :: API | e) m
                  => T.PackageName
                  -> m Unit
deleteQueueRemove pkgName = do
  client <- asks _.matrixClient
  liftAff (queueRemove client pkgName)

getLatestReportByPackageName :: forall a e m.
                                MonadReader { matrixClient :: MatrixApi | a } m
                             => MonadAff (api :: API | e) m
                             => T.PackageName
                             -> m (RD.RemoteData E.Error T.ShallowReport)
getLatestReportByPackageName pkgName = do
  client <- asks _.matrixClient
  shallowR <- liftAff $ attempt (latestReportByPackageName client pkgName)
  pure (RD.fromEither shallowR)

getPackageByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                 => MonadAff (api :: API | e) m
                 => T.PackageName
                 -> m (T.Package)
getPackageByName pkgName = do
  client <- asks _.matrixClient
  liftAff (packageByName client pkgName)

getSingleResult :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                => MonadAff (api :: API | e) m
                => T.PackageName
                -> T.Cell
                -> m (T.SingleResult)
getSingleResult pkgName cellName = do
  client <- asks _.matrixClient
  liftAff (singleResult client pkgName cellName)

putTagSaveByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
                     => MonadAff (api :: API | e) m
                     => T.PackageName
                     -> T.TagName
                     -> m Unit
putTagSaveByName pkgName tagName = do
  client <- asks _.matrixClient
  liftAff (tagSaveByName client tagName pkgName)

deleteTagRemove :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
               => MonadAff (api :: API | e) m
               => T.PackageName
               -> T.TagName
               -> m Unit
deleteTagRemove pkgName tagName = do
  client <- asks _.matrixClient
  liftAff (tagRemove client tagName pkgName)

getUserByName :: forall a e m. MonadReader { matrixClient :: MatrixApi | a } m
              => MonadAff (api :: API | e) m
              => T.Username
              -> m (RD.RemoteData E.Error T.User)
getUserByName user = do
  client <- asks _.matrixClient
  userName <- liftAff $ attempt (userByName client user)
  pure (RD.fromEither userName)

userByName :: forall e
  . MatrixApi
  -> T.Username
  -> Aff (api :: API | e) T.User
userByName api name = makeAff \err succ ->
  U.runFn4 userByName_ api name succ
    (stringyErr err "Getting user failed")

foreign import userByName_ :: forall eff .
  U.Fn4 MatrixApi
      T.Username
      (T.User      -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueByName :: forall e
  . MatrixApi
  -> T.PackageName
  -> Aff (api :: API | e) (Maybe T.QueueItem)
queueByName api pkgName = makeAff \err succ ->
  U.runFn9
    queueByName_
    api
    pkgName
    succ
    (stringyErr err "Getting queue item by name failed")
    "low"
    "medium"
    "high"
    Just
    Nothing

foreign import queueByName_ :: forall eff a .
  U.Fn9 MatrixApi
      T.PackageName
      (Maybe T.QueueItem -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      String
      String
      String
      (a -> Maybe a)
      (Maybe a)
      (ApiEff eff Unit)

queueSaveByName :: forall e
  . MatrixApi
  -> T.PackageName
  -> T.Priority
  -> Aff (api :: API | e) Unit
queueSaveByName api pkgName prio = makeAff \err succ ->
  U.runFn5
    queueSaveByName_
    api
    pkgName
    (case prio of
       T.Low -> "low"
       T.Medium -> "medium"
       T.High -> "high")
    succ
    (stringyErr err "Queue Build Failed")

foreign import queueSaveByName_ :: forall eff .
  U.Fn5 MatrixApi
      T.PackageName
      String
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueList :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (T.ApiList T.QueueItem)
queueList api = makeAff \err succ ->
  U.runFn3 queueList_ api succ
    (stringyErr err "Getting queue list failed")

foreign import queueList_ :: forall eff .
  U.Fn3 MatrixApi
      (T.ApiList T.QueueItem -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueCreate :: forall e
  . MatrixApi
  -> T.PackageName
  -> T.Priority
  -> Aff (api :: API | e) Unit
queueCreate api pkgName prio = makeAff \err succ ->
  U.runFn5
    queueCreate_
    api
    pkgName
    (case prio of
       T.Low -> "low"
       T.Medium -> "medium"
       T.High -> "high")
    succ
    (stringyErr err "Getting queue item by name failed")

foreign import queueCreate_ :: forall eff .
  U.Fn5 MatrixApi
      T.PackageName
      String
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueRemove :: forall e
  . MatrixApi
  -> T.PackageName
  -> Aff (api :: API | e) Unit
queueRemove api pkgName = makeAff \err succ ->
  U.runFn4
    queueRemove_
    api
    pkgName
    succ
    (stringyErr err "Remove Queue by name failed")

foreign import queueRemove_ :: forall eff .
  U.Fn4 MatrixApi
      T.PackageName
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagByName :: forall e
  . MatrixApi
  -> T.PackageName
  -> Aff (api :: API | e) (Maybe T.Tag)
tagByName api tagName = makeAff \err succ ->
  U.runFn6
    tagByName_
    api
    tagName
    succ
    (stringyErr err "Getting tag by name failed")
    Just
    Nothing

foreign import tagByName_ :: forall eff a .
  U.Fn6 MatrixApi
      T.TagName
      (Maybe T.Tag -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (a -> Maybe a)
      (Maybe a)
      (ApiEff eff Unit)

tagSaveByName :: forall e
  . MatrixApi
  -> T.TagName
  -> T.PackageName
  -> Aff (api :: API | e) Unit
tagSaveByName api pkgName tagName = makeAff \err succ ->
  U.runFn5
    tagSaveByName_
    api
    tagName
    pkgName
    succ
    (stringyErr err "Saving Tag by name failed")

foreign import tagSaveByName_ :: forall eff .
  U.Fn5 MatrixApi
      T.TagName
      T.PackageName
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagList :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (T.ApiList T.Tag)
tagList api = makeAff \err succ ->
  U.runFn3 tagList_ api succ
    (stringyErr err "Getting tag list failed")

foreign import tagList_ :: forall eff .
  U.Fn3 MatrixApi
      (T.ApiList T.Tag -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagRemove :: forall e
   . MatrixApi
  -> T.TagName
  -> T.PackageName
  -> Aff (api :: API | e) Unit
tagRemove api tagName pkgName = makeAff \err succ ->
  U.runFn5
    tagRemove_
    api
    tagName
    pkgName
    succ
    (stringyErr err "Removing Tag by name failed")

foreign import tagRemove_ :: forall eff .
  U.Fn5 MatrixApi
      T.TagName
      T.PackageName
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageTags :: forall e
   . MatrixApi
  -> T.PackageName
  -> T.Tag
  -> Aff (api :: API | e) T.Tag
packageTags api pkg tag = makeAff \err succ ->
  U.runFn5
    packageTags_
    api
    pkg
    tag
    succ
    (stringyErr err "Setting Tag failed")

foreign import packageTags_ :: forall eff .
  U.Fn5 MatrixApi
      T.PackageName
      T.Tag
      (T.Tag      -> ApiEff eff Unit)
      (JQueryXHR  -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageList :: forall e
   . MatrixApi
  -> T.Range
  -> Aff (api :: API | e) (T.ApiList T.PackageMeta)
packageList api range = makeAff \err succ ->
  U.runFn4
    packageList_
    api
    range
    (\pms -> succ $ pms { items = map packageMetaFromFFI pms.items })
    (stringyErr err "Getting package list failed")
  where
    packageMetaFromFFI :: PackageMetaFFI -> T.PackageMeta
    packageMetaFromFFI p = p { report = Misc.undefine p.report }

type PackageMetaFFI =
  { name   :: T.PackageName
  , report :: Undefined String
  , tags   :: Array T.TagName
  }

foreign import packageList_ :: forall eff .
  U.Fn4 MatrixApi
      T.Range
      (T.ApiList PackageMetaFFI -> ApiEff eff Unit)
      (JQueryXHR              -> ApiEff eff Unit)
      (ApiEff eff Unit)

listLatestReports :: forall e
   . MatrixApi
  -> T.Range
  -> Aff (api :: API | e) (T.ApiList T.LatestItem)
listLatestReports api range = makeAff \err succ ->
  U.runFn4
    listLatestReports_
    api
    range
    succ
    (stringyErr err "Getting latest reports list failed")

foreign import listLatestReports_ :: forall eff .
  U.Fn4 MatrixApi
      T.Range
      (T.ApiList T.LatestItem -> ApiEff eff Unit)
      (JQueryXHR              -> ApiEff eff Unit)
      (ApiEff eff Unit)


singleResult :: forall e
  . MatrixApi
  -> T.PackageName
  -> T.Cell
  -> Aff (api :: API | e) T.SingleResult
singleResult api pkgName cell = makeAff \err succ ->
  runFn14
    singleResult_
    api
    pkgName
    cell
    succ
    (stringyErr err "Getting single result failed")
    Nothing
    Just
    T.Ok
    T.Nop
    T.NoIp
    T.NoIpBjLimit
    T.NoIpFail
    T.Fail
    T.FailDeps

foreign import singleResult_ :: forall eff .
  Fn14
    MatrixApi
    T.PackageName
    T.Cell
    (T.SingleResult -> ApiEff eff Unit)
    (JQueryXHR    -> ApiEff eff Unit)
    (Maybe T.VersionResult)
    (T.VersionResult -> Maybe T.VersionResult)
    T.Result
    T.Result
    T.Result
    (Int -> T.Result)
    ({ err :: String, out :: String } -> T.Result)
    (String -> T.Result)
    (Array T.DepFailure -> T.Result)
    (ApiEff eff Unit)

latestReportByPackageName :: forall e
   . MatrixApi
  -> T.PackageName
  -> Aff (api :: API | e) T.ShallowReport
latestReportByPackageName api pkgName = makeAff \err succ ->
  runFn11
    latestReportByPackageName_
    api
    pkgName
    succ
    (stringyErr err "getting latest report by package name failed")
    T.ShallowOk
    T.ShallowNop
    T.ShallowNoIp
    T.ShallowNoIpBjLimit
    T.ShallowNoIpFail
    T.ShallowFail
    T.ShallowFailDeps

foreign import latestReportByPackageName_ :: forall eff .
  Fn11 MatrixApi
       T.PackageName
       (T.ShallowReport -> ApiEff eff Unit)
       (JQueryXHR     -> ApiEff eff Unit)
       T.ShallowResult
       T.ShallowResult
       T.ShallowResult
       (Int -> T.ShallowResult)
       T.ShallowResult
       T.ShallowResult
       (Int -> T.ShallowResult)
       (ApiEff eff Unit)

packageByName :: forall e
   . MatrixApi
  -> T.PackageName
  -> Aff (api :: API | e) T.Package
packageByName api pkgName = makeAff \err succ ->
  U.runFn7 packageByName_
    api
    pkgName
    succ
    (stringyErr err "getting package by name failed")
    T.Normal
    T.UnPreferred
    T.Deprecated

foreign import packageByName_ :: forall eff .
  U.Fn7 MatrixApi
      T.PackageName
      (T.Package -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      T.Preference
      T.Preference
      T.Preference
      (ApiEff eff Unit)

stringyErr :: forall e
   . (E.Error -> Eff (api :: API | e) Unit)
  -> String
  -> JQueryXHR
  -> Eff (api :: API | e) Unit
stringyErr err s = err <<< const (E.error s)

getVersionedPackageName :: Uri
                        -> Maybe { packageName :: T.PackageName, packageVersion :: T.VersionName }
getVersionedPackageName = toMaybe <<< getVersionedPackageName_

foreign import getVersionedPackageName_ :: Uri -> Nullable { packageName :: T.PackageName, packageVersion :: T.VersionName }

getPackageName :: Uri -> Maybe T.PackageName
getPackageName = toMaybe <<< getPackageName_

foreign import getPackageName_ :: Uri -> Nullable T.PackageName

foreign import data Fn11
  :: Type
  -> Type -> Type -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type -> Type
  -> Type

foreign import runFn11 :: forall a b c d e f g h i j k l
             . Fn11 a b c d e f g h i j k l
  -> a -> b -> c -> d -> e -> f
  -> g -> h -> i -> j -> k
  -> l

foreign import data Fn12
  :: Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type
  -> Type

foreign import runFn12 :: forall a b c d e f g h i j k l m
             . Fn12 a b c d e f g h i j k l m
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l
  -> m


foreign import data Fn13
  :: Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type -> Type
  -> Type

foreign import runFn13 :: forall a b c d e f g h i j k l m n
             . Fn13 a b c d e f g h i j k l m n
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l -> m
  -> n

foreign import data Fn14
  :: Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type
  -> Type -> Type -> Type -> Type -> Type -> Type
  -> Type

foreign import runFn14 :: forall a b c d e f g h i j k l m n o
             . Fn14 a b c d e f g h i j k l m n o
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l -> m -> n
  -> o
