{-# LANGUAGE TupleSections #-}
module Api.Utils
  ( listRange
  , filesByStamp
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.List        (sortBy)
import           Data.Text        (Text, pack)
import           Data.Time
import           Rest
import           System.Directory
import           System.FilePath

listRange :: Range -> [a] -> [a]
listRange r = take (count r) . drop (offset r)

filesByStamp :: ((FilePath, UTCTime) -> (FilePath, UTCTime) -> Ordering) -> (FilePath -> Bool) -> FilePath -> IO [(Text, UTCTime)]
filesByStamp cmp p dir
   =  fmap (map (first pack))
   .  fmap (sortBy cmp)
   .  mapM (\fp -> (fp,) <$> getModificationTime (dir </> fp))
  <=< fmap (filter p)
   $  getDirectoryContents dir
