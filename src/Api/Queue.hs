{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.Queue (resource) where

import           Control.Monad.Except
import           Data.String
import           Rest
import qualified Rest.Resource        as R

import           Api.Package          (validatePackage)
import           Api.Root             (Root, runDb)
import           Api.Types            (PackageName, WithPackage)
import           Api.Utils
import           Queue                (Create (..), Priority (..),
                                       QueueItem (..), queueItemToView)
import qualified Queue                as Q

resource :: Resource Root WithPackage PackageName () Void
resource = mkResourceReader
  { R.name   = "queue"
  , R.schema = withListing () $ named [("name", singleBy fromString)]
  , R.list   = const list
  , R.get    = Just get
  , R.create = Just create
  , R.update = Just update
  , R.remove = Just remove
  }

get :: Handler WithPackage
get = mkIdHandler jsonO $ const handler
  where
    handler :: PackageName -> ExceptT Reason_ WithPackage QueueItem
    handler pkgName = (`orThrow` NotFound) . fmap (fmap queueItemToView) .  runDb $ Q.byName pkgName

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [QueueItem]
    handler r = listRange r . fmap queueItemToView <$> runDb Q.list

create :: Handler Root
create = mkInputHandler jsonI handler
  where
    handler :: Create -> ExceptT Reason_ Root ()
    handler c = do
      secure
      validatePackage (cPackageName c)
      runDb $ Q.add (cPackageName c) (cPriority c) Nothing

update :: Handler WithPackage
update = mkIdHandler (jsonO . jsonI) handler
  where
    handler :: Priority -> PackageName -> ExceptT Reason_ WithPackage QueueItem
    handler prio pkgName = do
      secure
      validatePackage pkgName
      (`orThrow` NotFound) . fmap (fmap queueItemToView) . runDb $ do
        Q.setPriority pkgName prio
        Q.byName pkgName

remove :: Handler WithPackage
remove = mkIdHandler id $ const handler
  where
    handler :: PackageName -> ExceptT Reason_ WithPackage ()
    handler pkgName = do
      secure
      void $ runDb (Q.byName pkgName) `orThrow` NotFound
      void $ runDb $ Q.remove pkgName
