{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Paths (module Paths, module Path) where

import           Control.Monad.Catch
import qualified Data.ByteString.Lazy    as L
import           Data.String.Conversions
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Path
import           System.Directory
import qualified System.FilePath         as FP

instance ConvertibleStrings (Path b t) Text   where convertString = cs . toFilePath
instance ConvertibleStrings (Path b t) [Char] where convertString = toFilePath

createDirectoryP :: Path a Dir -> IO ()
createDirectoryP = createDirectory . toFilePath

removeFileP :: Path a File -> IO ()
removeFileP = removeFile . toFilePath

writeFileP :: Path a File -> String -> IO ()
writeFileP = writeFile . toFilePath

getDirectoryContentsP :: Path a Dir -> IO [FilePath]
getDirectoryContentsP = getDirectoryContents . toFilePath

removeDirectoryP :: Path a Dir -> IO ()
removeDirectoryP = removeDirectory . toFilePath

doesFileExistP :: Path a File -> IO Bool
doesFileExistP = doesFileExist . toFilePath

doesDirectoryExistP :: Path a Dir -> IO Bool
doesDirectoryExistP = doesDirectoryExist . toFilePath

getModificationTimeP :: Path a b -> IO UTCTime
getModificationTimeP = getModificationTime . toFilePath

lazyReadFileP :: Path a File -> IO LazyByteString
lazyReadFileP = L.readFile . toFilePath

lazyWriteFileP :: Path a File -> LazyByteString -> IO ()
lazyWriteFileP = L.writeFile . toFilePath

(<.>) :: MonadThrow m => Path Rel File -> String -> m (Path Rel File)
f <.> e = parseRelFile $ toFilePath f FP.<.> e

tryReadFile :: Path a File -> IO (Maybe LazyByteString)
tryReadFile fp = do
  exists <- doesFileExistP fp
  if not exists
    then return Nothing
    else Just <$> lazyReadFileP fp

tryReadFileWithModifiedTime :: Path a File -> IO (Maybe (UTCTime, LazyByteString))
tryReadFileWithModifiedTime fp = do
  exists <- doesFileExistP fp
  if not exists
    then return Nothing
    else fmap Just $ (,) <$> getModificationTimeP fp
                         <*> lazyReadFileP fp
