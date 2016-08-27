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

userByNameAff :: forall e
  . MatrixApi
  -> String
  -> Aff (api :: API | e) User
userByNameAff api name = makeAff \err succ ->
  runFn4 userByName api name succ
    (err <<< const (error "Getting user failed"))

foreign import userByName :: forall eff .
  Fn4 MatrixApi
      String
      (User      -> ApiEff eff Unit)
      (JQueryXHR -> ApiEff eff Unit)
      (ApiEff eff Unit)

tagListAff :: forall e
   . MatrixApi
  -> Aff (api :: API | e) (ApiList Tag)
tagListAff api = makeAff \err succ ->
  runFn3 tagList api succ (err <<< const (error "Getting tag list failed"))

foreign import tagList :: forall eff .
  Fn3 MatrixApi
      (ApiList Tag -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

packageListAff :: forall e
   . MatrixApi
  -> Range
  -> Aff (api :: API | e) (ApiList PackageMeta)
packageListAff api range = makeAff \err succ ->
  runFn4 packageList api range succ (err <<< const (error "Getting package list failed"))

foreign import packageList :: forall eff .
  Fn4 MatrixApi
      Range
      (ApiList PackageMeta -> ApiEff eff Unit)
      (JQueryXHR           -> ApiEff eff Unit)
      (ApiEff eff Unit)
