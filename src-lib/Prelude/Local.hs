{-# LANGUAGE FlexibleContexts  #-}

module Prelude.Local
    ( T.Text
    , Generic
    , FromJSON(..)
    , ToJSON(..)
    , myToJSON, myToEncoding, myParseJSON

    , C.simpleParse, C.display

    , POSIXTime
    , UTCTime
    , NominalDiffTime
    , getCurrentTime
    , getPOSIXTime
    , posixSecondsToUTCTime
    , diffUTCTime

    , InputStream, OutputStream

    , BS.ByteString
    , SBS.ShortByteString
    , Async
    , module Control.Concurrent.MVar
    , Set
    , Map
    , IntMap

    , module Control.Monad
    , module Control.DeepSeq
    , module Control.Monad.IO.Class
    , module Data.Bifunctor
    , module Data.List
    , module Data.Maybe
    , module Data.Proxy
    , module Data.Semigroup
    , module System.Directory
    , module System.Environment
    , module System.FilePath

    , module Prelude
    ) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Char (isUpper)
import           Data.IntMap (IntMap)
import           Data.List
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime,getCurrentTime, NominalDiffTime, diffUTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime,posixSecondsToUTCTime)
import qualified Distribution.Text as C
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Streams (InputStream, OutputStream)

myToJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
myToJSON = genericToJSON (defaultOptions { fieldLabelModifier = labelMod })

myToEncoding :: (Generic a, GToEncoding (Rep a)) => a -> Encoding
myToEncoding = genericToEncoding (defaultOptions { fieldLabelModifier = labelMod })

myParseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
myParseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = labelMod })

labelMod :: String -> String
labelMod = camelTo2 '_' . dropWhile (not . isUpper)
