{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module PkgId where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T
import           Data.Version
import           Distribution.Package
import           Distribution.Text
import           Control.DeepSeq

type PkgN = T.Text

newtype Ver = Ver [Int] deriving (Show,Eq,Ord,NFData)

instance Text Ver where
    disp = disp . verToVersion
    parse = do
        v <- parse
        maybe (fail "parse: invalid Ver") pure (verFromVersion v)

verToVersion :: Ver -> Version
verToVersion (Ver v) = makeVersion v

verFromVersion :: Version -> Maybe Ver
verFromVersion v
  | null (versionBranch v) = Nothing
  | otherwise = Just (Ver (versionBranch v))

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

instance Text PkgId where
    disp = disp . piFromPkgId
    parse = do
        p <- parse
        maybe (fail "parse: invalid PkgId") pure (piToPkgId p)

instance FromJSON PkgId where
    parseJSON = withText "PkgId" $ maybe (fail "invalid PkgId") pure . simpleParse . T.unpack

instance ToJSON PkgId where
    toJSON = toJSON . display

