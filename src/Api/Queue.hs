{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.Queue (resource) where

import           Control.Monad.Except
import           Rest
import qualified Rest.Resource        as R

import           Api.Root             (Root)
import           Api.Utils
import           Queue

resource :: Resource Root Root Void () Void
resource = mkResourceId
  { R.name   = "queue"
  , R.schema = withListing () $ named []
  , R.list   = const list
  }

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [QueueItem]
    handler r = listRange r <$> liftIO readQueue
