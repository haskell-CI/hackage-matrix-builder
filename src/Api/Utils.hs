{-# LANGUAGE TupleSections #-}
module Api.Utils
  ( listRange
  , filesByStamp
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Text        (Text, pack)
import           Data.Time
import           Rest
import           System.Directory
import           System.FilePath

listRange :: Range -> [a] -> [a]
listRange r = take (count r) . drop (offset r)

filesByStamp :: (FilePath -> Bool) -> FilePath -> IO [(Text, UTCTime)]
filesByStamp p dir
   =  fmap (map (first pack))
   .  mapM (\fp -> (fp,) <$> getModificationTime (dir </> fp))
  <=< fmap (filter p)
   $  getDirectoryContents dir
