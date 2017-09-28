{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Cli where

import           Options.Generic

-- TODO
data Opts
    = Init      -- ^ "Sync worker info into database"
    | Update    -- ^ "Perform package index sync/update"
    | Scheduler -- ^ "Run scheduling server"
    | WebServer -- ^ "Run HTTP server"
    | Compute   -- ^ "Run computation engine"
    deriving (Show, Generic)

instance ParseRecord Opts

getOpts :: IO Opts
getOpts = getRecord "hackage matrix builder (controller)"

-- main = print =<< getOpts
