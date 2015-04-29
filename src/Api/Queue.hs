{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Queue (resource) where

import           Control.Monad.Except
import           Data.List            (isPrefixOf)
import           Data.Ord
import           Data.Text            (Text)
import           Rest
import qualified Rest.Resource        as R

import           Api.Types
import           Api.Utils

resource :: Resource Root Root Void () Void
resource = mkResourceId
  { R.name   = "queue"
  , R.schema = withListing () $ named []
  , R.list   = const list
  }

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [Text]
    handler r = liftIO . fmap (listRange r . map fst) $ filesByStamp (comparing snd) (not . ("." `isPrefixOf`)) "queue"
