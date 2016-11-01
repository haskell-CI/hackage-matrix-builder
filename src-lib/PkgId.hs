{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData        #-}

{-# OPTIONS_GHC -Wall #-}

module PkgId where

import           Prelude.Local

import           Data.Aeson
import qualified Data.Text as T
import           Data.Version
import           Distribution.Package
import           Distribution.Text (disp, parse, display, simpleParse)
import qualified Distribution.Text as C

type PkgN = T.Text

newtype Ver = Ver [Int] deriving (Eq,Ord,NFData,Show)

instance C.Text Ver where
    disp = disp . verToVersion
    parse = do
        v <- parse
        maybe (fail "parse: invalid 'Ver'") pure (verFromVersion v)

instance FromJSON Ver where
    parseJSON = withText "Ver" $ maybe (fail "invalid 'Ver'") pure . simpleParse . T.unpack

instance ToJSON Ver where
    toJSON = toJSON . display

verToVersion :: Ver -> Version
verToVersion (Ver v) = makeVersion v

verFromVersion :: Version -> Maybe Ver
verFromVersion v
  | null (versionBranch v) = Nothing
  | otherwise = Just (Ver (versionBranch v))

----------------------------------------------------------------------------

data PkgId = PkgId !PkgN !Ver
           deriving (Ord,Eq,Show,Generic)

instance NFData PkgId

pkgIdVersion :: PkgId -> Version
pkgIdVersion (PkgId _ v) = verToVersion v

piToPkgId :: PackageIdentifier -> Maybe PkgId
piToPkgId (PackageIdentifier (PackageName n) v)
  | not (null n), Just v' <- verFromVersion v = Just (PkgId (T.pack n) v')
  | otherwise = Nothing

piFromPkgId :: PkgId -> PackageIdentifier
piFromPkgId (PkgId n v) = PackageIdentifier (PackageName $ T.unpack n) (verToVersion v)

instance C.Text PkgId where
    disp = disp . piFromPkgId
    parse = do
        p <- parse
        maybe (fail "parse: invalid PkgId") pure (piToPkgId p)

instance FromJSON PkgId where
    parseJSON = withText "PkgId" $ maybe (fail "invalid PkgId") pure . simpleParse . T.unpack

instance ToJSON PkgId where
    toJSON = toJSON . display

