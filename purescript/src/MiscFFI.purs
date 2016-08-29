module MiscFFI where

import Control.Monad.Eff
import Control.Monad.Eff.JQuery
import DOM
import DOM.HTML.Types
import Data.Foreign.Null
import Data.Function.Uncurried
import Prelude
import Unsafe.Coerce

foreign import onPopstate :: forall e
   . (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit

delegate :: forall e a b
   . (Selectable a, Selectable b)
  => a
  -> b
  -> String -- e.g. "click"
  -> (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit
delegate = runFn4 delegate_

class Selectable a where
  e :: a -> a
instance selectableHTMLElement :: Selectable HTMLElement where
  e = id
instance selectableJQuery      :: Selectable JQuery where
  e = id
instance selectableString      :: Selectable String where
  e = id

foreign import delegate_ :: forall e a b
   . (Selectable a, Selectable b)
  => Fn4 a
         b
         String -- e.g. "click"
         (JQueryEvent -> Eff (dom :: DOM | e) Unit)
         (Eff (dom :: DOM | e) Unit)


selectElement :: forall e
   . HTMLElement
  -> Eff (dom :: DOM | e) JQuery
selectElement = select <<< unsafeCoerce

foreign import eventTarget :: forall e . JQueryEvent -> Eff (dom :: DOM | e) HTMLElement
foreign import altKey      :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import ctrlKey     :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import metaKey     :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import shiftKey    :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import which       :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Int

-- String is probably too lenient in he return type!
foreign import getAttr :: forall e . String -> JQuery -> Eff (dom :: DOM | e) String
