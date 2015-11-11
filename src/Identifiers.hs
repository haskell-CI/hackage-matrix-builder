{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Identifiers where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.JSON.Schema
import           Data.String
import           Data.String.Conversions
import           Rest.Info
import           Rest.ShowUrl
import           Text.Read

import           Api.Root                (Root)

newtype PackageName = PackageName { unPackageName :: StrictText }
 deriving (FromJSON, Eq, IsString, JSONSchema, Ord, Show, ToJSON, Read, ShowUrl)
instance Info PackageName where
  describe _ = "identifier"

instance ConvertibleStrings PackageName StrictText where convertString = unPackageName
instance ConvertibleStrings PackageName String where convertString = cs . unPackageName

type WithPackage = ReaderT PackageName Root

newtype VersionName = VersionName { unVersionName :: StrictText }
  deriving (Eq, FromJSON, IsString, JSONSchema, Ord, Show, ToJSON)

-- TODO These should use a smart constructor validating the format.
instance ConvertibleStrings StrictText VersionName where
  convertString = VersionName
instance ConvertibleStrings String VersionName where
  convertString = VersionName . cs

newtype Revision = Revision { unRevision :: Word }
  deriving (Eq, FromJSON, JSONSchema, Ord, Show, ToJSON)

instance ConvertibleStrings String (Maybe Revision) where
  convertString = fmap Revision . readMaybe

newtype TagName = TagName { unTagName :: StrictText }
  deriving (Eq, FromJSON, JSONSchema, Ord, Read, Show, ToJSON)
