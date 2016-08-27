module MatrixApi where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe
import Data.Foreign.Null
import Data.Function.Uncurried
import Data.Maybe
import Prelude
import Types

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
  runFn4 packageList_ api range succ
    (stringyErr err "Getting package list failed")

foreign import packageList_ :: forall eff .
  Fn4 MatrixApi
      Range
      (ApiList PackageMeta -> ApiEff eff Unit)
      (JQueryXHR           -> ApiEff eff Unit)
      (ApiEff eff Unit)

stringyErr :: forall e
   . (Error -> Eff (api :: API | e) Unit)
  -> String
  -> JQueryXHR
  -> Eff (api :: API | e) Unit
stringyErr err s = err <<< const (error "Getting user failed")
