module Lib.MatrixApi where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Lib.MiscFFI
import Data.Function.Uncurried
import Data.Maybe
import Data.Nullable
import Prelude
import Lib.Types
import Lib.Undefined
import Lib.Uri (Uri)
import Control.Monad.Reader (ReaderT)

type MyMatrixApi e = ReaderT { matrixClient :: MatrixApi } (Aff e)

type MatrixApis eff = MyMatrixApi (api :: API | eff)

foreign import data MatrixApi :: Type

foreign import data API :: Effect

foreign import newApi :: forall eff
   . String
  -> String
  -> Eff (api :: API | eff) MatrixApi

foreign import data JQueryXHR :: Type

type ApiEff e o = Eff (api :: API | e) o

userByName :: forall e
  . MatrixApi
  -> String
  -> Aff (api :: API | e) User
userByName api name = makeAff \err succ ->
  runFn4 userByName_ api name succ
    (stringyErr err "Getting user failed")

foreign import userByName_ :: forall eff .
  Fn4 MatrixApi
      String
      (User      -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueByName :: forall e
  . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) (Maybe QueueItem)
queueByName api pkgName = makeAff \err succ ->
  runFn9
    queueByName_
    api
    pkgName
    succ
    (stringyErr err "Getting queue item by name failed")
    Low
    Medium
    High
    Just
    Nothing

foreign import queueByName_ :: forall eff a .
  Fn9 MatrixApi
      PackageName
      (Maybe QueueItem -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      Priority
      Priority
      Priority
      (a -> Maybe a)
      (Maybe a)
      (ApiEff eff Unit)

queueSaveByName :: forall e
  . MatrixApi
  -> PackageName
  -> QueueItem
  -> Aff (api :: API | e) Unit
queueSaveByName api pkgName queueItem = makeAff \err succ ->
  runFn5
    queueSaveByName_
    api
    pkgName
    queueItem
    succ
    (stringyErr err "Saving Tag by name failed")

foreign import queueSaveByName_ :: forall eff .
  Fn5 MatrixApi
      PackageName
      QueueItem
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueList :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (ApiList QueueItem)
queueList api = makeAff \err succ ->
  runFn3 queueList_ api succ
    (stringyErr err "Getting queue list failed")

foreign import queueList_ :: forall eff .
  Fn3 MatrixApi
      (ApiList QueueItem -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

queueCreate :: forall e
  . MatrixApi
  -> PackageName
  -> Priority
  -> Aff (api :: API | e) Unit
queueCreate api pkgName prio = makeAff \err succ ->
  runFn5
    queueCreate_
    api
    pkgName
    (case prio of
       Low -> "low"
       Medium -> "medium"
       high -> "high")
    succ
    (stringyErr err "Getting queue item by name failed")

foreign import queueCreate_ :: forall eff .
  Fn5 MatrixApi
      PackageName
      String
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagByName :: forall e
  . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) (Maybe Tag)
tagByName api tagName = makeAff \err succ ->
  runFn6
    tagByName_
    api
    tagName
    succ
    (stringyErr err "Getting tag by name failed")
    Just
    Nothing

foreign import tagByName_ :: forall eff a .
  Fn6 MatrixApi
      TagName
      (Maybe Tag -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (a -> Maybe a)
      (Maybe a)
      (ApiEff eff Unit)

tagSaveByName :: forall e
  . MatrixApi
  -> PackageName
  -> Tag
  -> Aff (api :: API | e) Unit
tagSaveByName api pkgName tagName = makeAff \err succ ->
  runFn5
    tagSaveByName_
    api
    pkgName
    tagName
    succ
    (stringyErr err "Saving Tag by name failed")

foreign import tagSaveByName_ :: forall eff .
  Fn5 MatrixApi
      PackageName
      Tag
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagList :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (ApiList Tag)
tagList api = makeAff \err succ ->
  runFn3 tagList_ api succ
    (stringyErr err "Getting tag list failed")

foreign import tagList_ :: forall eff .
  Fn3 MatrixApi
      (ApiList Tag -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagRemove :: forall e
   . MatrixApi
  -> PackageName
  -> Tag
  -> Aff (api :: API | e) Unit
tagRemove api pkgName tagName = makeAff \err succ ->
  runFn5
    tagRemove_
    api
    pkgName
    tagName
    succ
    (stringyErr err "Removing Tag by name failed")

foreign import tagRemove_ :: forall eff .
  Fn5 MatrixApi
      PackageName
      Tag
      (Unit -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageTags :: forall e
   . MatrixApi
  -> PackageName
  -> Tag
  -> Aff (api :: API | e) Tag
packageTags api pkg tag = makeAff \err succ ->
  runFn5
    packageTags_
    api
    pkg
    tag
    succ
    (stringyErr err "Setting Tag failed")

foreign import packageTags_ :: forall eff .
  Fn5 MatrixApi
      PackageName
      Tag
      (Tag        -> ApiEff eff Unit)
      (JQueryXHR  -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageList :: forall e
   . MatrixApi
  -> Range
  -> Aff (api :: API | e) (ApiList PackageMeta)
packageList api range = makeAff \err succ ->
  runFn4
    packageList_
    api
    range
    (\pms -> succ $ pms { items = map packageMetaFromFFI pms.items })
    (stringyErr err "Getting package list failed")
  where
    packageMetaFromFFI :: PackageMetaFFI -> PackageMeta
    packageMetaFromFFI p = p { report = undefine p.report }

type PackageMetaFFI =
  { name   :: PackageName
  , report :: Undefined String
  , tags   :: Array TagName
  }

foreign import packageList_ :: forall eff .
  Fn4 MatrixApi
      Range
      (ApiList PackageMetaFFI -> ApiEff eff Unit)
      (JQueryXHR              -> ApiEff eff Unit)
      (ApiEff eff Unit)

listLatestReports :: forall e
   . MatrixApi
  -> Range
  -> Aff (api :: API | e) (ApiList LatestItem)
listLatestReports api range = makeAff \err succ ->
  runFn4
    listLatestReports_
    api
    range
    succ
    (stringyErr err "Getting latest reports list failed")

foreign import listLatestReports_ :: forall eff .
  Fn4 MatrixApi
      Range
      (ApiList LatestItem -> ApiEff eff Unit)
      (JQueryXHR          -> ApiEff eff Unit)
      (ApiEff eff Unit)
  

singleResult :: forall e
  . MatrixApi
  -> PackageName
  -> Cell
  -> Aff (api :: API | e) SingleResult
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
    Ok
    Nop
    NoIp
    NoIpBjLimit
    NoIpFail
    Fail
    FailDeps

foreign import singleResult_ :: forall eff .
  Fn14
    MatrixApi
    PackageName
    Cell
    (SingleResult -> ApiEff eff Unit)
    (JQueryXHR    -> ApiEff eff Unit)
    (Maybe VersionResult)
    (VersionResult -> Maybe VersionResult)
    Result
    Result
    Result
    (Int -> Result)
    ({ err :: String, out :: String } -> Result)
    (String -> Result)
    (Array DepFailure -> Result)
    (ApiEff eff Unit)

latestReportByPackageName :: forall e
   . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) ShallowReport
latestReportByPackageName api pkgName = makeAff \err succ ->
  runFn11
    latestReportByPackageName_
    api
    pkgName
    succ
    (stringyErr err "getting latest report by package name failed")
    ShallowOk
    ShallowNop
    ShallowNoIp
    ShallowNoIpBjLimit
    ShallowNoIpFail
    ShallowFail
    ShallowFailDeps

foreign import latestReportByPackageName_ :: forall eff .
  Fn11 MatrixApi
       PackageName
       (ShallowReport -> ApiEff eff Unit)
       (JQueryXHR     -> ApiEff eff Unit)
       ShallowResult
       ShallowResult
       ShallowResult
       (Int -> ShallowResult)
       ShallowResult
       ShallowResult
       (Int -> ShallowResult)
       (ApiEff eff Unit)

packageByName :: forall e
   . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) Package
packageByName api pkgName = makeAff \err succ ->
  runFn7 packageByName_
    api
    pkgName
    succ
    (stringyErr err "getting package by name failed")
    Normal
    UnPreferred
    Deprecated

foreign import packageByName_ :: forall eff .
  Fn7 MatrixApi
      PackageName
      (Package -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      Preference
      Preference
      Preference
      (ApiEff eff Unit)

stringyErr :: forall e
   . (Error -> Eff (api :: API | e) Unit)
  -> String
  -> JQueryXHR
  -> Eff (api :: API | e) Unit
stringyErr err s = err <<< const (error s)

getVersionedPackageName :: Uri -> Maybe { packageName :: PackageName, packageVersion :: VersionName }
getVersionedPackageName = toMaybe <<< getVersionedPackageName_

foreign import getVersionedPackageName_ :: Uri -> Nullable { packageName :: PackageName, packageVersion :: VersionName }

getPackageName :: Uri -> Maybe PackageName
getPackageName = toMaybe <<< getPackageName_

foreign import getPackageName_ :: Uri -> Nullable PackageName

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
