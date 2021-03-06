module Lib.MiscFFI where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, select)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement, Window)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Prelude (Unit, otherwise, id, show, pure, unit, (/=), (<<<), (<>), (<$>), (*), (==), (<),(>))
import Unsafe.Coerce (unsafeCoerce)
import Lib.Types as T
import Data.Argonaut as Arg
import Data.Traversable as TR
import Data.Tuple as Tuple
import Data.Tuple.Nested as TupleN
import Data.String as Str
import Network.RemoteData as RD
import Data.Array as Arr
import Control.Monad.Eff.Exception as E
import Data.Formatter.DateTime as FDT
import Data.Time.Duration (Milliseconds(Milliseconds)) as DT
import Data.DateTime (DateTime) as DT
import Data.DateTime.Instant (instant, toDateTime) as DT
import Data.Int as Int
import Data.Ordering

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

foreign import posixSecondsToISOStr :: Int -> String

toDateTime :: T.PkgIdxTs -> T.PackageTS
toDateTime = posixSecondsToISOStr



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

makeTuplePkgVer :: T.PackageName
                -> T.VersionName
                -> T.HCVer
                -> T.InitialPackage
makeTuplePkgVer pkg ver hcver=
  let
    ghcV = (Str.takeWhile ((/=) '@') hcver)
    ghc = if Str.null hcver then Nothing else pure ghcV
    ver' = if Str.null ver then Nothing else pure ver
    pkgts  = ((Str.drop 1) <<< Str.dropWhile ((/=) '@')) hcver
  in TupleN.tuple4 pkg pkgts ver' ghc

makeTuplePkgIdx :: T.PackageName
                -> T.InitialPackage
makeTuplePkgIdx pkgName =
  let
    pkg = (Str.takeWhile ((/=) '@') pkgName)
    pkgts  = ((Str.drop 1) <<< Str.dropWhile ((/=) '@')) pkgName
  in TupleN.tuple4 pkg pkgts Nothing Nothing


getLastIdx :: Array Int -> Int
getLastIdx arrInt=
  case Arr.last arrInt of
    Just a -> a
    Nothing -> 0

showPrio :: Int -> String
showPrio x
  | x < 0     = "low"
  | x == 0    = "medium"
  | otherwise = "high"

foreign import scrollMaxY :: forall e. Window -> Eff (dom :: DOM | e) Int
