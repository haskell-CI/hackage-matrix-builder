module MatrixApi where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Data.Foreign.Undefined
import MiscFFI
import Data.Function.Uncurried
import Data.Maybe
import Data.Nullable
import Prelude
import Types
import Uri (Uri)

foreign import data MatrixApi :: *

foreign import data API :: !

foreign import newApi :: forall eff
   . String
  -> String
  -> Eff (api :: API | eff) MatrixApi

foreign import data JQueryXHR :: *

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
  :: *
  -> * -> * -> * -> * -> * -> *
  -> * -> * -> * -> * -> *
  -> *

foreign import runFn11 :: forall a b c d e f g h i j k l
             . Fn11 a b c d e f g h i j k l
  -> a -> b -> c -> d -> e -> f
  -> g -> h -> i -> j -> k
  -> l

foreign import data Fn12
  :: *
  -> * -> * -> * -> *
  -> * -> * -> * -> *
  -> * -> * -> * -> *
  -> *

foreign import runFn12 :: forall a b c d e f g h i j k l m
             . Fn12 a b c d e f g h i j k l m
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l
  -> m


foreign import data Fn13
  :: *
  -> * -> * -> * -> *
  -> * -> * -> * -> *
  -> * -> * -> * -> * -> *
  -> *

foreign import runFn13 :: forall a b c d e f g h i j k l m n
             . Fn13 a b c d e f g h i j k l m n
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l -> m
  -> n

foreign import data Fn14
  :: *
  -> * -> * -> * -> *
  -> * -> * -> * -> *
  -> * -> * -> * -> * -> * -> *
  -> *

foreign import runFn14 :: forall a b c d e f g h i j k l m n o
             . Fn14 a b c d e f g h i j k l m n o
  -> a -> b -> c -> d
  -> e -> f -> g -> h
  -> i -> j -> k -> l -> m -> n
  -> o
