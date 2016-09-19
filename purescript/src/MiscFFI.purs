module MiscFFI where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.JQuery
import DOM
import DOM.HTML.Types
import Data.Foreign.Null
import Data.Function.Uncurried
import Prelude
import Uri (Uri)
import Uri as Uri
import Unsafe.Coerce

foreign import onPopstate :: forall e
   . (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit

foreign import delegate2 :: forall e a b
   . (Selectable a, Selectable b)
  => a
  -> b
  -> String -- e.g. "click"
  -> (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> (Eff (dom :: DOM | e) Unit)

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

foreign import target :: forall e . JQueryEvent -> Eff (dom :: DOM | e) HTMLElement
foreign import altKey      :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import ctrlKey     :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import metaKey     :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import shiftKey    :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Boolean
foreign import which       :: forall e . JQueryEvent -> Eff (dom :: DOM | e) Int

-- String is probably too lenient in he return type!
foreign import getAttr :: forall e . String -> JQuery -> Eff (dom :: DOM | e) String

foreign import unsafeLog :: forall a e . a -> Eff (console :: CONSOLE | e) Unit

foreign import delay :: forall e . Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM | e) Unit

historyPushState :: forall e . String -> Uri -> Eff (dom :: DOM | e) Unit
historyPushState t u = runFn2 historyPushState_ t (Uri.toString u)

foreign import historyPushState_ :: forall e
  . Fn2 String
        String
        (Eff (dom :: DOM | e) Unit)

historyReplaceState :: forall e . String -> Uri -> Eff (dom :: DOM | e) Unit
historyReplaceState t u = runFn2 historyReplaceState_ t (Uri.toString u)

foreign import historyReplaceState_ :: forall e
  . Fn2 String
        String
        (Eff (dom :: DOM | e) Unit)

foreign import setDocumentTitle :: forall e . String -> Eff (dom :: DOM | e) Unit
