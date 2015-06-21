{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import           Data.Time
import           Generics.Generic.Aeson
import           Path
import           System.Directory

import           Api.Types               (PackageName (..), strip)
import           Api.Utils
import           BuildTypes
import qualified System.FilePath         as F

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
   .  mapM (get . PackageName)
  =<< getDirWithFilter (not . ("." `isPrefixOf`)) $(mkRelDir "queue")

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
path = ("queue" F.</>) . cs

data QueueItem = QueueItem
  { qPackageName :: PackageName
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

data Priority
  = Low
  | Medium
  | High
  deriving (Bounded, Eq, Enum, Generic, Ord, Read, Show)
instance ToJSON     Priority where toJSON    = gtoJson
instance FromJSON   Priority where parseJSON = gparseJson
instance JSONSchema Priority where schema    = gSchema
