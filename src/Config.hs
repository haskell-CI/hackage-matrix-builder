{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Data.Text (Text)

data Config = Config
  { sqliteDb          :: Text
  , jsClientTarget    :: Text
  , authFile          :: Text
  , authUser          :: Text
  , authPass          :: Text
  , uiConfigFile      :: Text
  , packagesJson      :: Text
  , packageNamesJson  :: Text
  , webServerPort     :: Int
  , webServerHostName :: Text
  , reportDir         :: Text
  } deriving Show

defaultConfig :: IO Config
defaultConfig = return Config
  { sqliteDb          = "db.sqlite"
  , jsClientTarget    = "ui/api.js"
  , authFile          = "auth"
  , authUser          = "trustee"
  , authPass          = "1234"
  , uiConfigFile      = "ui/config.js"
  , packagesJson      = "packages.json"
  , packageNamesJson  = "packageNames.json"
  , webServerPort     = 3000
  , webServerHostName = "127.0.0.1"
  , reportDir         = "report"
  }
