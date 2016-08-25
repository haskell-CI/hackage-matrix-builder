{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.Queue (resource) where

import           Control.Monad.Except
import           Data.String
import           Rest
import           Rest.Container       (List)
import qualified Rest.Resource        as R

import           Api.Package          (validatePackage)
import           Api.Root             (Root)
import           Api.Types            (PackageName, WithPackage)
import           Api.Utils
import           Queue                (Create (..), Priority (..),
                                       QueueItem (..))
import qualified Queue                as Q

resource :: Resource Root WithPackage PackageName Void ()
resource = mkResourceReader
  { R.name   = "queue"
  , R.schema = noListing $ named [ ("name", singleBy fromString)
                                 , ("list", static ())
                                 ]
  , R.get    = Just get
  , R.create = Just create
  , R.update = Just update
  , R.remove = Just remove
  , R.statics = \case
      () -> list
  }

get :: Handler WithPackage
get = mkIdHandler jsonO $ const handler
  where
    handler :: PackageName -> ExceptT Reason_ WithPackage QueueItem
    handler pkgName = (`orThrow` NotFound) . liftIO $ Q.get pkgName

list :: Handler Root
list = mkUnlimitedListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root (List QueueItem)
    handler r = unlimitedListRange r <$> liftIO Q.list

create :: Handler Root
create = mkInputHandler jsonI handler
  where
    handler :: Create -> ExceptT Reason_ Root ()
    handler c = do
      secure
      validatePackage (cPackageName c)
      liftIO $ Q.add (cPackageName c) (cPriority c)

update :: Handler WithPackage
update = mkIdHandler (jsonO . jsonI) handler
  where
    handler :: Priority -> PackageName -> ExceptT Reason_ WithPackage QueueItem
    handler prio pkgName = do
      secure
      validatePackage pkgName
      (`orThrow` NotFound) . liftIO $ Q.update pkgName prio

remove :: Handler WithPackage
remove = mkIdHandler id $ const handler
  where
    handler :: PackageName -> ExceptT Reason_ WithPackage ()
    handler pkgName = do
      secure
      void $ liftIO (Q.get pkgName) `orThrow` NotFound
      void $ liftIO $ Q.remove pkgName
