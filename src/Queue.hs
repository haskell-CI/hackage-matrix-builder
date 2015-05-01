{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Queue
  ( QueueItem (..)
  , Priority (..)
  , readQueue
  , addToQueue
  ) where

import           Control.Arrow
import           Data.Aeson.Utils
import qualified Data.ByteString.Lazy   as L
import           Data.JSON.Schema
import           Data.List              (isPrefixOf, sortBy)
import           Data.Maybe
import           Data.Ord
import           Data.Text              (unpack)
import           Data.Time
import           Generics.Generic.Aeson
import           System.Directory

import           Api.Utils              (filesByStamp)
import           BuildTypes

addToQueue :: String -> Priority -> IO ()
addToQueue pkg prio = do
  createDirectoryIfMissing True "queue"
  L.writeFile ("queue/" ++ pkg) (encode prio)

readQueue :: IO [QueueItem]
readQueue
   =  fmap (sortBy (comparing $ (negate . fromEnum . qPriority &&& qModified) &&& qPackageName))
   .  (mapM $ \(fp,ts) -> QueueItem fp ts . fromMaybe Medium . decodeV <$> L.readFile ("queue/" ++ unpack fp))
  =<< filesByStamp (not . ("." `isPrefixOf`)) "queue"

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
