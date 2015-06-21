module Api.Tag (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.String.Conversions
import           Rest
import qualified Rest.Resource           as R

import           Api.Package             (validatePackage)
import           Api.Root
import           Api.Types               (PackageName)
import           Api.Utils
import           Tag

type WithTag = ReaderT TagName Root

resource :: Resource Root WithTag TagName () Void
resource = mkResourceReader
  { R.name   = "tag"
  , R.schema = withListing () $ named [ ("name"   , singleBy (normalizeTagName . cs))
                                      ]
  , R.get    = Just get
  , R.list   = const list
  , R.update = Just update
  , R.remove = Just remove
  }

get :: Handler WithTag
get = mkIdHandler jsonO $ const handler
  where
    handler :: TagName -> ExceptT Reason_ WithTag Tag
    handler tagName =
      Tag.byName tagName `orThrow` NotFound

list :: ListHandler Root
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ Root [Tag]
    handler r = listRange r <$> toList

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
