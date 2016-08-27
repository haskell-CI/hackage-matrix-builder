module MatrixApi where

import Control.Monad.Eff
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

foreign import userByName :: forall eff
   . MatrixApi
  -> String
  -> (User      -> ApiEff eff Unit)
  -> (JQueryXHR -> ApiEff eff Unit)
  -> ApiEff eff Unit

foreign import tagList :: forall eff .
  Fn3 MatrixApi
      (ApiList Tag -> ApiEff eff Unit)
      (JQueryXHR   -> ApiEff eff Unit)
      (ApiEff eff Unit)

foreign import packageList :: forall eff .
  Fn5 MatrixApi
      (Maybe Int)
      (Maybe Int)
      (ApiList PackageMeta -> ApiEff eff Unit)
      (JQueryXHR           -> ApiEff eff Unit)
      (ApiEff eff Unit)
