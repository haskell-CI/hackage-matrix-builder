{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall #-}

module PkgId
    ( PkgN(..)
    , pkgNFromPackageName
    , pkgNFromText

    , Ver(..)
    , verFromVersion
    , alterVer
    , mkVer

    , PkgId(..)
    , pkgIdFromPackageIdentifier

    , UnitID(..), unUnitID
    , unitIDFromUnitId

    , CompilerID, compilerVer
    , compilerIDFromCompilerId

    , PkgIdxTs(..), unPkgIdxTs
    , PkgRev
    ) where

import           Prelude.Local

import           Data.Aeson
import           Data.Aeson.Types                     (toJSONKeyText)
import qualified Data.List.NonEmpty                   as NE
import           Data.String
import qualified Data.Text                            as T
import           Data.Vector.Unboxed.Deriving         (derivingUnbox)
import           Distribution.Compiler                (CompilerFlavor (..),
                                                       CompilerId (..))
import           Distribution.Package
import           Distribution.Text                    (disp, display, parse,
                                                       simpleParse)
import qualified Distribution.Text                    as C
import           Distribution.Version

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import           Data.Swagger
import           Servant.API

----------------------------------------------------------------------------

newtype PkgN = PkgN PackageName deriving (Eq,Ord,NFData,C.Text)

instance Show PkgN where
    showsPrec p x
      | p >= 11    = (("(PkgN \""++display x++"\")") ++)
      | otherwise  = (("PkgN \""++display x++"\"") ++)

instance FromJSON PkgN where
    parseJSON = withText "PkgN" $ maybe (fail "invalid 'PkgN'") pure . simpleParse . T.unpack

instance ToJSON PkgN where
    toJSON = toJSON . display

-- TODO: validation
instance FromField PkgN where
    fromField f dat = fromString <$> fromField f dat

instance ToField PkgN where
    toField = toField . display

instance Hashable PkgN where
    hashWithSalt s = hashWithSalt s . display

pkgNFromPackageName :: PackageName -> Maybe PkgN
pkgNFromPackageName pn
  | pn == mkPackageName ""  = Nothing
  | otherwise               = Just (PkgN pn)

pkgNFromText :: Text -> Maybe PkgN
pkgNFromText = simpleParse . T.unpack

instance IsString PkgN where
    -- TODO: validatation!
    fromString s
      | Just pn <- simpleParse s =  PkgN pn
      | otherwise                =  error "IsString(PkgN): invalid package name"

instance FromHttpApiData PkgN where
    parseUrlPiece = maybe (Left (T.pack "invalid pkg-name")) Right . simpleParse . T.unpack

instance ToParamSchema PkgN where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString

instance ToSchema PkgN where
    declareNamedSchema _ = pure $ NamedSchema (Just "PkgName") $ mempty
        & type_ .~ SwaggerString
        & example ?~ toJSON (PkgN "lens")

----------------------------------------------------------------------------

newtype Ver = Ver Version deriving (Eq,Ord,NFData,C.Text)

instance Show Ver where
    showsPrec p x
      | p >= 11    = (("(Ver \""++display x++"\")") ++)
      | otherwise  = (( "Ver \""++display x++"\"") ++)

-- instance C.Text Ver where
--     disp = disp . verToVersion
--     parse = do
--         v <- parse
--         maybe (fail "parse: invalid 'Ver'") pure (verFromVersion v)

instance FromJSON Ver where
    parseJSON = withText "Ver" $ maybe (fail "invalid 'Ver'") pure . simpleParse . T.unpack

instance ToJSON Ver where
    toJSON = toJSON . display

instance ToJSONKey Ver where
    toJSONKey = toJSONKeyText (T.pack . display)

instance FromJSONKey Ver where
    fromJSONKey = FromJSONKeyTextParser (maybe (fail "Ver") pure . simpleParse . T.unpack)

instance FromField Ver where
    fromField f dat = (maybe (error "FromField(Ver)") id . simpleParse) <$> fromField f dat

instance ToField Ver where
    toField = toField . display

instance ToSchema Ver where
    declareNamedSchema _ = pure $ NamedSchema (Just "Version") $ mempty
        & type_ .~ SwaggerString
        & example ?~ toJSON (mkVer (4 :| [15,3]))

instance ToParamSchema Ver where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString

instance Hashable Ver where
    hashWithSalt s (Ver vs) = hashWithSalt s (versionNumbers vs)

verFromVersion :: Version -> Maybe Ver
verFromVersion v
  | null (versionNumbers v) = Nothing
  | otherwise = Just (Ver v)

alterVer :: ([Int] -> [Int]) -> Ver -> Ver
alterVer f (Ver v) = Ver (alterVersion f v)

mkVer :: NonEmpty Int -> Ver
mkVer = Ver . mkVersion . NE.toList

----------------------------------------------------------------------------

data PkgId = PkgId !PkgN !Ver
           deriving (Ord,Eq,Show,Generic)

instance NFData PkgId

instance FromField PkgId where
    fromField f dat = (maybe (error "FromField(PkgId)") id . simpleParse) <$> fromField f dat

instance ToField PkgId where
    toField = toField . display

-- pkgIdVersion :: PkgId -> Version
-- pkgIdVersion (PkgId _ v) = verToVersion v

piToPkgId, pkgIdFromPackageIdentifier :: PackageIdentifier -> Maybe PkgId
piToPkgId (PackageIdentifier n v) =
    PkgId <$> pkgNFromPackageName n <*> verFromVersion v

pkgIdFromPackageIdentifier = piToPkgId

piFromPkgId :: PkgId -> PackageIdentifier
piFromPkgId (PkgId (PkgN pn) (Ver v)) = PackageIdentifier pn v

instance C.Text PkgId where
    disp = disp . piFromPkgId
    parse = do
        p <- parse
        maybe (fail "parse: invalid PkgId") pure (piToPkgId p)

instance FromJSON PkgId where
    parseJSON = withText "PkgId" $ maybe (fail "invalid PkgId") pure . simpleParse . T.unpack

instance ToJSON PkgId where
    toJSON = toJSON . display

----------------------------------------------------------------------------

-- c.f. Cabal's UnitId
newtype UnitID = UnitID Text -- opaque-ish identifier
               deriving (Show,Eq,Ord,FromJSON,ToJSON,ToField,FromField,ToJSONKey,FromJSONKey)

unUnitID :: UnitID -> Text
unUnitID (UnitID t) = t


unitIDFromUnitId :: UnitId -> UnitID
unitIDFromUnitId = UnitID . T.pack . unUnitId

----------------------------------------------------------------------------

newtype CompilerID = CompilerID {- ghc/ghcjs/ghcvm -} Ver
                   deriving (Show,Eq,Ord,NFData)

compilerVer :: CompilerID -> Ver
compilerVer (CompilerID v) = v

compilerIDFromCompilerId :: CompilerId -> Maybe CompilerID
compilerIDFromCompilerId (CompilerId GHC v) = CompilerID <$> verFromVersion v
compilerIDFromCompilerId _                  = Nothing

instance C.Text CompilerID where
    disp = disp . PkgId (PkgN (mkPackageName "ghc")) . compilerVer
    parse = do
        p <- parse
        maybe (fail "parse: invalid CompilerId") pure (compilerIDFromCompilerId p)

instance FromJSON CompilerID where
    parseJSON = withText "CompilerID" $ maybe (fail "invalid CompilerId") pure . simpleParse . T.unpack

instance ToJSON CompilerID where
    toJSON = toJSON . display

instance ToHttpApiData CompilerID where
    toUrlPiece = T.pack . display

instance FromHttpApiData CompilerID where
    parseUrlPiece = maybe (Left $ T.pack "invalid CompilerId") Right . simpleParse . T.unpack

instance FromField CompilerID where
    fromField f dat = (maybe (error "invalid CompilerId") id . simpleParse) <$> fromField f dat

instance ToField CompilerID where
    toField = toField . display

----------------------------------------------------------------------------

-- simpler for now (i.e. until year 2038 brings everything down); 'Word' would be more accurate
newtype PkgIdxTs = PkgIdxTs Int
    deriving (Show,Ord,Eq,NFData,FromJSON,ToJSON,FromField,ToField,FromHttpApiData,ToHttpApiData,Hashable)

unPkgIdxTs :: PkgIdxTs -> Int
unPkgIdxTs (PkgIdxTs i) = i

instance ToSchema PkgIdxTs where
    declareNamedSchema _ = pure $ NamedSchema (Just "IdxState") $ mempty
        & type_ .~ SwaggerInteger
        & example ?~ toJSON (PkgIdxTs 1491048000)
        & description ?~ "Seconds elapsed since 1970-01-01T00:00:00Z"
        & minimum_ ?~ 0
        & maximum_ ?~ 0x7fffffff

instance ToParamSchema PkgIdxTs where
    toParamSchema _ = mempty
        & type_ .~ SwaggerInteger
        & minimum_ ?~ 0
        & maximum_ ?~ 0x7fffffff

derivingUnbox "PkgIdxTs" [t| PkgIdxTs -> Int |] [| \(PkgIdxTs x) -> x |] [| PkgIdxTs |]

----------------------------------------------------------------------------

type PkgRev = Word
