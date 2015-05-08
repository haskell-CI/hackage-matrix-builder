{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Queue (Priority (..)) where

import           Control.Monad
import           Data.Aeson.Utils
import           Data.JSON.Schema
import           Data.String.Conversions
import           Database.Persist
import           Database.Persist.Sql
import           Generics.Generic.Aeson
import           GHC.Generics

import           Api.Types               (PackageName (..))

data Priority
  = Low
  | Medium
  | High
  deriving (Bounded, Eq, Enum, Generic, Ord, Read, Show)
instance ToJSON     Priority where toJSON    = gtoJson
instance FromJSON   Priority where parseJSON = gparseJson
instance JSONSchema Priority where schema    = gSchema

instance PersistField Priority where
  toPersistValue = PersistInt64 . \case
    Low    -> 1
    Medium -> 2
    High   -> 3
  fromPersistValue = intPriority <=< fromPersistValue
    where
      intPriority :: Int -> Either StrictText Priority
      intPriority = \case
        1 -> Right Low
        2 -> Right Medium
        3 -> Right High
        e -> Left $ cs $ "Expected priority but got: " <> show e
instance PersistFieldSql Priority where
  sqlType _ = SqlInt64

instance PersistField PackageName where
  toPersistValue = PersistText . unPackageName
  fromPersistValue = fmap PackageName . fromPersistValue
instance PersistFieldSql PackageName where
  sqlType _ = SqlString
