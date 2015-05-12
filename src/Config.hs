{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Config where

import           Control.Monad.Catch
import qualified Data.ByteString.Lazy    as L
import           Data.String.Conversions
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Path
import           System.Directory
import qualified System.FilePath         as FP

data Config = Config
  { sqliteDb          :: Path Rel File
  , jsClientTarget    :: Path Rel File
  , authFile          :: Path Rel File
  , authUser          :: Text
  , authPass          :: Text
  , uiConfigFile      :: Path Rel File
  , packagesJson      :: Path Rel File
  , packageNamesJson  :: Path Rel File
  , webServerPort     :: Int
  , webServerHostName :: Text
  , reportDir         :: Path Rel Dir
  } deriving Show

defaultConfig :: IO Config
defaultConfig = return Config
  { sqliteDb          = $(mkRelFile "db.sqlit")
  , jsClientTarget    = $(mkRelFile "ui/api.js")
  , authFile          = $(mkRelFile "auth")
  , authUser          = "trustee"
  , authPass          = "1234"
  , uiConfigFile      = $(mkRelFile "ui/config.js")
  , packagesJson      = $(mkRelFile "packages.json")
  , packageNamesJson  = $(mkRelFile "packageNames.json")
  , webServerPort     = 3000
  , webServerHostName = "127.0.0.1"
  , reportDir         = $(mkRelDir "report")
  }

instance ConvertibleStrings (Path b t) Text   where convertString = cs . toFilePath
instance ConvertibleStrings (Path b t) [Char] where convertString = toFilePath

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

(<.>) :: MonadThrow m => Path Rel File -> String -> m (Path Rel File)
f <.> e = parseRelFile $ toFilePath f FP.<.> e
