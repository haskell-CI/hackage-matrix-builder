module Lib.MiscFFI where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.JQuery
import DOM
import DOM.HTML.Types
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable
import Prelude
import Lib.Uri (Uri)
import Lib.Uri as Uri
import Lib.Undefined
import Unsafe.Coerce
import Lib.Types as T
import Data.Argonaut as Arg
import Data.Traversable as TR
import Data.Tuple as Tuple
import Network.RemoteData as RD
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response as Affjax
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Data.Int as Int
import Data.Array as Arr


foreign import onPopstate :: forall e
   . (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit

foreign import delegate2 :: forall e a b
   . Selectable a
  => Selectable b
  => a
  -> b
  -> String -- e.g. "click"
  -> (JQueryEvent -> Eff (dom :: DOM | e) Unit)
  -> (Eff (dom :: DOM | e) Unit)

delegate :: forall e a b
   . Selectable a
  => Selectable b
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
   . Selectable a
  => Selectable b
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

foreign import unsafeTrace :: forall a . a -> a

foreign import delay :: forall e . Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM | e) Unit

foreign import val :: forall e . JQuery -> Eff (dom :: DOM | e) String

foreign import is :: forall e . String -> JQuery -> Eff (dom :: DOM | e) Boolean

historyPushState :: forall e . String -> Uri -> Eff (dom :: DOM | e) Unit
historyPushState t u = runFn2 historyPushState_ t (Uri.toString u)

foreign import historyPushState_ :: forall e .
  Fn2 String
      String
      (Eff (dom :: DOM | e) Unit)

historyReplaceState :: forall e . String -> Uri -> Eff (dom :: DOM | e) Unit
historyReplaceState t u = runFn2 historyReplaceState_ t (Uri.toString u)

foreign import historyReplaceState_ :: forall e .
  Fn2 String
      String
      (Eff (dom :: DOM | e) Unit)

foreign import setDocumentTitle :: forall e . String -> Eff (dom :: DOM | e) Unit

autocomplete :: forall e
   . { source :: Array String
     , select :: ({ item :: { value :: String } }
              -> Eff (dom :: DOM | e) Unit)
     }
  -> JQuery
  -> Eff (dom :: DOM | e) Unit
autocomplete s jq = runFn3 autocomplete_ jq s.source s.select

foreign import autocomplete_ :: forall e.
  Fn3 JQuery
      (Array String)
      ({ item :: { value :: String } }
        -> Eff (dom :: DOM | e) Unit)
      (Eff (dom :: DOM | e) Unit)

foreign import formatDate_ :: String -> String

formatDate :: Maybe String -> String
formatDate date =
  case date of
    Just dt -> formatDate_ dt
    Nothing -> ""

undefine :: forall a . Undefined a -> Maybe a
undefine u = undefine_ u Nothing (Just unit)

foreign import undefine_ :: forall a b . Undefined a -> Maybe b -> Maybe b -> Maybe a

foreign import tabs :: forall e . JQuery -> Eff (dom :: DOM | e) Unit

lookupIndex :: T.PackageName -> Array (Tuple.Tuple T.PackageName String) -> String
lookupIndex pkgName pkgIdxTuple =
  case Tuple.lookup pkgName pkgIdxTuple of
    Just a  -> "@" <> a
    Nothing -> ""

fromIndexToNumber :: Maybe Arg.JArray -> Array Number
fromIndexToNumber (Just arrJson) =
  case TR.traverse Arg.toNumber arrJson of
    Just arrStr -> arrStr
    Nothing      -> []
fromIndexToNumber Nothing        = []


