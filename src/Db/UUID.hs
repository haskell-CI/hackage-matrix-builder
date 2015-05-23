{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Db.UUID (UUID, nextRandom) where

import           Data.String.Conversions
import           Data.UUID
import           Data.UUID.Aeson         ()
import           Data.UUID.V4            (nextRandom)
import           Database.Esqueleto
import           Web.PathPieces

instance ConvertibleStrings UUID String             where convertString = toString
instance ConvertibleStrings String (Maybe UUID)     where convertString = fromString
instance ConvertibleStrings UUID StrictText         where convertString = cs . toString
instance ConvertibleStrings StrictText (Maybe UUID) where convertString = fromString . cs

instance PersistField UUID where
  toPersistValue = toPersistValue . toString
  fromPersistValue = \case
    PersistText s -> maybe (Left "PersistValue UUID: Invalid format") Right $ cs s
    _             -> Left "PersistValue UUID: Not a string"

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PathPiece UUID where
  fromPathPiece = fromString . cs
  toPathPiece   = cs . toString
