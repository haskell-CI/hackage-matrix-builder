{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Queue
  ( QueueItem (..)
  , Create (..)
  , queueItemToView
  , queueItemToView'
  , module Db.Queue
  , module Types.Queue
  ) where

import           Data.Aeson.Utils
import           Data.JSON.Schema
import           Data.Time
import           Database.Esqueleto
import           Generics.Generic.Aeson

import           Api.Types              (PackageName (..), strip)
import           BuildTypes
import           Db.Queue
import           Types.Queue

queueItemToView :: Entity Queue -> QueueItem
queueItemToView (Entity _ q) = queueItemToView' q

queueItemToView' :: Queue -> QueueItem
queueItemToView' q = QueueItem
  { qPackageName = queuePackageName q
  , qCreated     = queueCreated q
  , qModified    = queueModified q
  , qPriority    = queuePriority q
  }

data QueueItem = QueueItem
  { qPackageName :: PackageName
  , qCreated     :: UTCTime
  , qModified    :: UTCTime
  , qPriority    :: Priority
  } deriving (Eq, Generic, Show)

instance ToJSON     QueueItem where toJSON    = gtoJsonWithSettings    $ strip "q"
instance FromJSON   QueueItem where parseJSON = gparseJsonWithSettings $ strip "q"
instance JSONSchema QueueItem where schema    = gSchemaWithSettings    $ strip "q"

data Create = Create
  { cPackageName :: PackageName
  , cPriority    :: Priority
  } deriving (Eq, Generic, Show)
instance ToJSON     Create where toJSON    = gtoJsonWithSettings    $ strip "c"
instance FromJSON   Create where parseJSON = gparseJsonWithSettings $ strip "c"
instance JSONSchema Create where schema    = gSchemaWithSettings    $ strip "c"
