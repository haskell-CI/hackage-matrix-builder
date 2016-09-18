module Uri where

import Control.Monad.Eff
import DOM
import DOM.HTML.Types
import Data.Foreign.Null
import Data.Function.Uncurried
import Prelude

-- | Creation and conversion

foreign import data Uri :: *

foreign import windowUri :: forall e . Eff (dom :: DOM | e) String

-- This assumes that Uri is defined globally, which it is by default.
foreign import newUri :: String -> Uri

foreign import toString :: Uri -> String

-- If we ever want to pass a Uri back to JS we need to make sure we keep persistance internally.
foreign import clone :: Uri -> Uri

-- | Getters

-- All getters (except query! But lets be consistent) may return null if the value isn't set so we use
-- Language.FFI.Nullable here which converts null -> Null and String -> Nullable String.
-- Nullable is distinguished from Maybe to not break haskell compatibility.
-- If we want to decode null into Nullable (Nullable a) there is no way of
-- knowing if Null or Nullable Null is correct. This problem does not exist when
-- working with Maybe values in client server communication.
foreign import protocol :: Uri -> Null String

foreign import userInfo :: Uri -> Null String

foreign import host :: Uri -> Null String

foreign import port :: Uri -> Null String

foreign import path :: Uri -> Null String

foreign import query :: Uri -> Null String

foreign import anchor :: Uri -> Null String

-- | Other getters

foreign import queryParamValue :: String -> Uri -> String

foreign import queryParamValues :: String -> Uri -> Array String

-- | Setters

-- We could use Null here to combine the with* and remove* functions
-- but usage would be more verbose that way.

-- JsUri has clone() conveniently defined so we use it to get
-- persistence, otherwise our types would be `-> Eff (dom :: DOM) Uri`
-- which is worse to work with.

foreign import withProtocol :: String -> Uri -> Uri

foreign import withUserInfo :: String -> Uri -> Uri

foreign import withHost :: String -> Uri -> Uri

foreign import withPort :: String -> Uri -> Uri

foreign import withPath :: String -> Uri -> Uri

foreign import withQuery :: String -> Uri -> Uri

foreign import withAnchor :: String -> Uri -> Uri

-- | Removals

foreign import removeProtocol :: Uri -> Uri

foreign import removeUserInfo :: Uri -> Uri

foreign import removeHost :: Uri -> Uri

foreign import removePort :: Uri -> Uri

foreign import removePath :: Uri -> Uri

foreign import removeQuery :: Uri -> Uri

foreign import removeAnchor :: Uri -> Uri


-- | Other setters

foreign import addQueryParam :: String -> String -> Uri -> Uri

foreign import replaceQueryParam :: String -> String -> Uri -> Uri

-- The order of the arguments differ from the jsUri api, it is now
-- key -> oldValue -> newValue -> Uri -> Uri
foreign import replaceQueryParamValue :: String -> String -> String -> Uri -> Uri

foreign import deleteQueryParam :: String -> Uri -> Uri

foreign import deleteQueryParamValue :: String -> String -> Uri -> Uri

-- | PureScript stuff

instance showUri :: Show Uri where
  show u = "newUri \"" <> toString u <> "\""
