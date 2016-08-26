module MatrixApi where

import Prelude (Unit)
import Types

import Control.Monad.Eff (Eff)

foreign import data MatrixApi :: *

foreign import data API :: !

foreign import newApi :: forall eff
   . String
  -> String
  -> Eff (api :: API | eff) MatrixApi

foreign import data JQueryXHR :: *

foreign import userByName :: forall eff
   . MatrixApi
  -> String
  -> (User -> Eff (api :: API | eff) Unit)
  -> (JQueryXHR -> Eff (api :: API | eff) Unit)
  -> Eff (api :: API | eff) Unit

foreign import tagList :: forall eff
   . MatrixApi
  -> (ApiList Tag -> Eff (api :: API | eff) Unit)
  -> (JQueryXHR -> Eff (api :: API | eff) Unit)
  -> Eff (api :: API | eff) Unit
