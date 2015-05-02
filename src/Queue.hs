{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Queue
  ( QueueItem (..)
  , Priority (..)
  , Create (..)

  , get
  , list
  , add
  , update
  , remove

  , path
  ) where

import           Control.Arrow
import           Data.Aeson.Utils
import qualified Data.ByteString         as S
import           Data.JSON.Schema
import           Data.List               (isPrefixOf, sortBy)
import           Data.Maybe
import           Data.Ord
import           Data.String.Conversions
import           Data.Text               (unpack)
import           Data.Time
import           Generics.Generic.Aeson
import           System.Directory
import           System.FilePath         ((</>))

import           Api.Utils               (getDirWithFilter)
import           BuildTypes

type PackageName = Text

get :: PackageName -> IO (Maybe QueueItem)
get p = do
  let fp = path p
  de <- doesFileExist fp
  if de
    then do
      ts   <- getModificationTime fp
      prio <- fromMaybe Medium . decodeV . cs <$> S.readFile fp
      return $ Just QueueItem
        { qPackageName = p
        , qModified    = ts
        , qPriority    = prio
        }
    else return Nothing

list :: IO [QueueItem]
list
   =  fmap (sortBy (comparing $ (negate . fromEnum . qPriority &&& qModified) &&& qPackageName))
   .  fmap catMaybes
   .  mapM get
  =<< getDirWithFilter (not . ("." `isPrefixOf`)) "queue"

add :: PackageName -> Priority -> IO ()
add pkg prio = S.writeFile (path pkg) (cs $ encode prio)

update :: PackageName -> Priority -> IO (Maybe QueueItem)
update pkgName prio = do
  mq <- get pkgName
  case mq of
    Nothing -> return Nothing
    Just q -> do
      add pkgName prio
      ts <- getModificationTime $ path pkgName
      return $ Just q
        { qPriority = prio
        , qModified = ts
        }

remove :: PackageName -> IO Bool
remove pkg = do
  mq <- get pkg
  maybe (return False) ((>> return True) . removeFile . path . qPackageName) mq

path :: PackageName -> FilePath
path = ("queue" </>) . unpack

data Priority
  = Low
  | Medium
  | High
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)
instance ToJSON     Priority where toJSON    = gtoJson
instance FromJSON   Priority where parseJSON = gparseJson
instance JSONSchema Priority where schema    = gSchema

data QueueItem = QueueItem
  { qPackageName :: Text
  , qModified    :: UTCTime
  , qPriority    :: Priority
  } deriving (Eq, Generic, Show)

instance ToJSON     QueueItem where toJSON    = gtoJsonWithSettings    queueItemSettings
instance FromJSON   QueueItem where parseJSON = gparseJsonWithSettings queueItemSettings
instance JSONSchema QueueItem where schema    = gSchemaWithSettings    queueItemSettings
queueItemSettings :: Settings
queueItemSettings = Settings { stripPrefix = Just "q" }

data Create = Create
  { cPackageName :: Text
  , cPriority    :: Priority
  } deriving (Eq, Generic, Show)
instance ToJSON     Create where toJSON    = gtoJsonWithSettings    createSettings
instance FromJSON   Create where parseJSON = gparseJsonWithSettings createSettings
instance JSONSchema Create where schema    = gSchemaWithSettings    createSettings
createSettings :: Settings
createSettings = Settings { stripPrefix = Just "c" }
