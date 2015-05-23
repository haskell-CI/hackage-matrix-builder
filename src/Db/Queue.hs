{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Db.Queue where

import           Control.Monad.Reader
import           Data.Time
import           Database.Esqueleto
import           Database.Persist.TH

import           Api.Types            (PackageName (..))
import           Db.UUID
import           Types.Queue          (Priority (..))

share [mkPersist sqlSettings, mkMigrate "migrateQueue"] [persistLowerCase|
Queue
    myId UUID
    Primary myId
    packageName PackageName
    priority    Priority
    created     UTCTime default=CURRENT_TIME
    modified    UTCTime default=CURRENT_TIME
    UniquePackageName packageName
    deriving Show
|]

byName :: MonadIO m => PackageName -> SqlPersistT m (Maybe (Entity Queue))
byName = getBy . UniquePackageName

list :: MonadIO m => SqlPersistT m [Entity Queue]
list =
  select $ from $ \q -> do
    orderBy [desc (q ^. QueuePriority), asc (q ^. QueueCreated)]
    return q

add :: MonadIO m => PackageName -> Priority -> Maybe UTCTime -> SqlPersistT m ()
add pkg prio mtime = do
  t <- maybe (liftIO getCurrentTime) return mtime
  u <- liftIO nextRandom
  insert_ Queue
    { queueMyId        = u
    , queuePackageName = pkg
    , queuePriority    = prio
    , queueCreated     = t
    , queueModified    = t
    }

setPriority :: MonadIO m => PackageName -> Priority -> SqlPersistT m ()
setPriority pkg prio =
  update $ \q -> do
    set q [QueuePriority =. val prio]
    where_ $ q ^. QueuePackageName ==. val pkg

remove :: MonadIO m => PackageName -> SqlPersistT m ()
remove pkg =
  delete $ from $ \(q :: SqlExpr (Entity Queue)) ->
    where_ $ q ^. QueuePackageName ==. val pkg
