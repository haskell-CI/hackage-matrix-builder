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
  runFn4 packageList_ api range
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

latestReportByPackageName :: forall e
   . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) Report
latestReportByPackageName api pkgName = makeAff \err succ ->
  runFn4 latestReportByPackageName_
    api
    pkgName
    succ
    (stringyErr err "getting latest report by package name failed")

foreign import latestReportByPackageName_ :: forall eff .
  Fn4 MatrixApi
      PackageName
      (Report    -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageByName :: forall e
   . MatrixApi
  -> PackageName
  -> Aff (api :: API | e) Package
packageByName api pkgName = makeAff \err succ ->
  runFn4 packageByName_
    api
    pkgName
    succ
    (stringyErr err "getting package by name failed")

foreign import packageByName_ :: forall eff .
  Fn4 MatrixApi
      PackageName
      (Package   -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

stringyErr :: forall e
   . (Error -> Eff (api :: API | e) Unit)
  -> String
  -> JQueryXHR
  -> Eff (api :: API | e) Unit
stringyErr err s = err <<< const (error "Getting user failed")

getVersionedPackageName :: Uri -> Maybe { packageName :: PackageName, packageVersion :: VersionName }
getVersionedPackageName = toMaybe <<< getVersionedPackageName_

foreign import getVersionedPackageName_ :: Uri -> Nullable { packageName :: PackageName, packageVersion :: VersionName }

getPackageName :: Uri -> Maybe PackageName
getPackageName = toMaybe <<< getPackageName_

foreign import getPackageName_ :: Uri -> Nullable PackageName
