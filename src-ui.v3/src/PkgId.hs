{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module PkgId
    ( PkgN(..)
    , TagN(..)
    , Matches(..)
    , matchesEmpty
    , pkgNFromText

    , Ver
    , verToText
    , verFromText
--    , verFromVersion
--    , alterVer
--    , mkVer

    -- , PkgId(..)
    -- , pkgIdFromPackageIdentifier

    -- , UnitID(..), unUnitID
    -- , unitIDFromUnitId

    , CompilerID, compilerVer
    , compilerIdFromText
    , compilerIdToText

    , PkgIdxTs(..), pkgIdxTsToText -- , unPkgIdxTs
    , PkgRev

    , UserName
    ) where

import           Control.Monad                (fail)
import           Data.Aeson                   (FromJSON (..), FromJSONKey (..),
                                               ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson                   as J
import qualified Data.Aeson.Types             as J
import qualified Data.Char                    as C
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock.POSIX        (POSIXTime, posixSecondsToUTCTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import qualified Data.Version                 as Ver
import           Servant.API                  (FromHttpApiData (..),
                                               ToHttpApiData (..))
import           Text.ParserCombinators.ReadP (readP_to_S)

type UserName   = Text
type PkgRev     = Word

----------------------------------------------------------------------------

newtype PkgN = PkgN { pkgNToText :: Text }
  deriving (Eq,Ord,FromJSON,ToJSON,ToJSONKey,FromJSONKey,FromHttpApiData,ToHttpApiData)

instance Show PkgN where
    showsPrec p x
      | p >= 11    = (("(PkgN "<>show x<>")") <>)
      | otherwise  = (("PkgN "<>show x) <>)

-- NB: this assumes the Hackage ascii-only policy
pkgNFromText :: Text -> Maybe PkgN
pkgNFromText t0
  | isValid t0 = Just (PkgN t0)
  | otherwise  = Nothing
  where
    isValid t
      | T.null t = False
      | not (T.all (\c -> C.isAsciiLower c || C.isAsciiUpper c || C.isDigit c || c == '-') t) = False
      | otherwise = and [ T.any C.isAlpha x | x <- T.split (=='-') t ]

----------------------------------------------------------------------------

newtype CompilerID = CompilerID {- ghc/ghcjs/ghcvm -} Ver
                   deriving (Show,Eq,Ord)

compilerVer :: CompilerID -> Ver
compilerVer (CompilerID v) = v

compilerIdFromText :: Text -> Maybe CompilerID
compilerIdFromText t = do
  vs <- T.stripPrefix (T.pack "ghc-") t
  CompilerID <$> verFromText vs

compilerIdToText :: CompilerID -> Text
compilerIdToText (CompilerID v) = T.pack "ghc-" <> verToText v

instance FromJSON CompilerID where
    parseJSON = J.withText "CompilerID" $ maybe (fail "invalid CompilerId") pure . compilerIdFromText

instance ToJSON CompilerID where
    toJSON = toJSON . compilerIdToText

instance ToJSONKey CompilerID where
    toJSONKey = J.toJSONKeyText compilerIdToText

instance FromJSONKey CompilerID where
    fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "CompilerID") pure . compilerIdFromText)

instance ToHttpApiData CompilerID where
    toUrlPiece = compilerIdToText

instance FromHttpApiData CompilerID where
    parseUrlPiece = maybe (Left $ T.pack "invalid CompilerId") Right . compilerIdFromText

----------------------------------------------------------------------------

newtype PkgIdxTs = PkgIdxTs Int
    deriving (Show,Ord,Eq,FromJSON,ToJSON,FromHttpApiData,ToHttpApiData)

pkgIdxTsToText :: PkgIdxTs -> Text
pkgIdxTsToText (PkgIdxTs t) = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%TZ" (posixSecondsToUTCTime (fromIntegral t :: POSIXTime))

----------------------------------------------------------------------------

newtype Ver = Ver [Int]
            deriving (Eq,Ord,Show)

instance FromJSON Ver where
  parseJSON o = (Ver . Ver.versionBranch) <$> J.parseJSON o

instance FromJSONKey Ver where
  fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "Ver") pure . verFromText)

instance ToHttpApiData Ver where
  toUrlPiece = verToText

verFromText :: Text -> Maybe Ver
verFromText t = case filter (null . snd) . readP_to_S Ver.parseVersion . T.unpack $ t of
                  [(v,[])] -> Just (Ver (Ver.versionBranch v))
                  _        -> Nothing

verToText :: Ver -> Text
verToText (Ver x) = T.pack . Ver.showVersion . Ver.makeVersion $ x

-- parseVerText :: Text -> J.Parser Ver
-- parseVerText = go . readP_to_S parseVersion . unpack
--   where
--     go [(v,[])] = return v
--     go (_ : xs) = go xs
--     go _        = fail "could not parse Version"

----------------------------------------------------------------------------
newtype TagN = TagN { tagNToText :: Text }
  deriving (Eq,Ord,FromJSON,ToJSON,ToJSONKey,FromJSONKey,FromHttpApiData,ToHttpApiData)

data Matches = Matches 
  { matchesInput :: Text
  , matchesExact :: Map.Map Text ()
  , matchesInfix :: Map.Map Text (Text,Text,Text)
  }
  deriving (Eq,Ord)

matchesEmpty :: Matches
matchesEmpty = Matches { matchesInput = T.empty :: T.Text, matchesExact = Map.empty, matchesInfix = Map.empty} 

  