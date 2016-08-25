{-# LANGUAGE LambdaCase #-}
module Api.Tag (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.String.Conversions
import           Rest
import           Rest.Container          (List)
import qualified Rest.Resource           as R

import           Api.Package             (validatePackage)
import           Api.Root
import           Api.Types               (PackageName)
import           Api.Utils
import           Tag

type WithTag = ReaderT TagName Root

resource :: Resource Root WithTag TagName Void ()
resource = mkResourceReader
  { R.name   = "tag"
  , R.schema = noListing $ named [ ("name" , singleBy (normalizeTagName . cs))
                                 , ("list" , static ())
                                 ]
  , R.get    = Just get
  , R.update = Just update
  , R.remove = Just remove
  , R.statics = \case
      () -> list
  }

get :: Handler WithTag
get = mkIdHandler jsonO $ const handler
  where
    handler :: TagName -> ExceptT Reason_ WithTag Tag
    handler tagName =
      Tag.byName tagName `orThrow` NotFound

list :: Handler Root
list = mkUnlimitedListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root (List Tag)
    handler r = unlimitedListRange r <$> toList

update :: Handler WithTag
update = mkIdHandler (jsonO . jsonI) handler
  where
    handler :: PackageName -> TagName -> ExceptT Reason_ WithTag ()
    handler p t = do
      secure
      validatePackage p
      addTaggedPackage p t

remove :: Handler WithTag
remove = mkIdHandler (jsonO . jsonI) handler
  where
    handler :: PackageName -> TagName -> ExceptT Reason_ WithTag ()
    handler p t = do
      secure
      validatePackage p
      removed <- removeTaggedPackage p t
      unless removed $ throwError NotFound
