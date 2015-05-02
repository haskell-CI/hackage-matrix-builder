{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.Queue (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text            (Text, pack)
import           Rest
import qualified Rest.Resource        as R

import           Api.Root             (Root)
import           Api.Utils
import           Queue                (Create (..), Priority, QueueItem)
import qualified Queue                as Q

type WithPackage = ReaderT Text Root

resource :: Resource Root WithPackage Text () Void
resource = mkResourceReader
  { R.name   = "queue"
  , R.schema = withListing () $ named [("name", singleBy pack)]
  , R.list   = const list
  , R.get    = Just get
  , R.create = Just create
  , R.update = Just update
  , R.remove = Just remove
  }

get :: Handler WithPackage
get = mkIdHandler jsonO $ const handler
  where
    handler :: Text -> ExceptT Reason_ WithPackage QueueItem
    handler pkgName = liftIO (Q.get pkgName) `orThrow` NotFound

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [QueueItem]
    handler r = listRange r <$> liftIO Q.list

create :: Handler Root
create = mkInputHandler jsonI handler
  where
    handler :: Create -> ExceptT Reason_ Root ()
    handler c = do
      secure
      liftIO $ Q.add (cPackageName c) (cPriority c)

update :: Handler WithPackage
update = mkIdHandler (jsonO . jsonI) handler
  where
    handler :: Priority -> Text -> ExceptT Reason_ WithPackage QueueItem
    handler prio pkgName = do
      secure
      liftIO (Q.update pkgName prio) `orThrow` NotFound

remove :: Handler WithPackage
remove = mkIdHandler id $ const handler
  where
    handler :: Text -> ExceptT Reason_ WithPackage ()
    handler pkgName = do
      secure
      void $ liftIO (Q.get pkgName) `orThrow` NotFound
      void . liftIO $ Q.remove pkgName
