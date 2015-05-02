{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
module Api.Utils
  ( listRange
  , filesByStamp
  , getDirWithFilter
  , secure
  , strip
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import           Data.List.Split         (splitOn)
import qualified Data.Map                as Map
import           Data.Text               (Text, pack)
import           Data.Time
import           Generics.Generic.Aeson
import           Happstack.Server.Auth   (basicAuth)
import           Happstack.Server.Monads
import           Rest
import           System.Directory
import           System.FilePath
import           System.IO

listRange :: Range -> [a] -> [a]
listRange r = take (count r) . drop (offset r)

filesByStamp :: (FilePath -> Bool) -> FilePath -> IO [(Text, UTCTime)]
filesByStamp p dir
   =  fmap (map (first pack))
   .  mapM (\fp -> (fp,) <$> getModificationTime (dir </> fp))
  <=< fmap (filter p)
   $  getDirectoryContents dir

getDirWithFilter :: (FilePath -> Bool) -> FilePath -> IO [Text]
getDirWithFilter p = fmap (map pack . filter p) . getDirectoryContents

secure :: Happstack m => ExceptT (Reason a) m ()
secure = do
  login <- liftIO . fmap (splitOn "/") . readFile $ "auth"
  case login of
    [u,p] -> do
      lift $ basicAuth "localhost" (Map.fromList [(u, p)]) $ return ()
    _ -> do
      liftIO $ hPutStrLn stderr "Failure reading auth file"
      throwError Busy

strip :: String -> Settings
strip = Settings . Just
