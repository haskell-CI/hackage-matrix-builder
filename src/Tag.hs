{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Tag (module Tag, TagName (..)) where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString         as SB
import           Data.JSON.Schema
import           Data.List
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                as S
import           Data.String.Conversions
import qualified Data.Text               as T
import           Generics.Generic.Aeson

import           Api.Types               (PackageName)
import           BuildTypes              hiding (PkgVerStatus (..))
import           Config
import           Identifiers             (TagName (..))
import           Paths

data Tag = Tag
  { name     :: TagName
  , packages :: Set PackageName
  } deriving (Eq, Generic, Show)

instance ToJSON     Tag where toJSON    = gtoJson
instance FromJSON   Tag where parseJSON = gparseJson
instance JSONSchema Tag where schema    = gSchema

newtype Tags = Tags { unTags :: Map TagName (Set PackageName) }
  deriving (Eq, Show)

instance Monoid Tags where
  mempty  = Tags M.empty
  (Tags a) `mappend` (Tags b) = Tags $ M.unionWith S.union a b

singleton :: TagName -> PackageName -> Tags
singleton t p = Tags (M.singleton t (S.singleton p))

instance FromJSON Tags where
  parseJSON = fmap (Tags . M.mapKeys normalizeTagName) . parseJSON
instance ToJSON Tags where
  toJSON = toJSON . M.mapKeys unTagName . unTags

loadTags :: (MonadConfig m, MonadIO m) => m Tags
loadTags = do
  fmap (fromMaybe (Tags M.empty) . decode . cs) . liftIO . SB.readFile . toFilePath =<< asksConfig tagsFile

writeTags :: (MonadConfig m, MonadIO m) => Tags -> m ()
writeTags tags = do
  p <- asksConfig tagsFile
  liftIO . lazyWriteFileP p . encode $ tags

byName :: (MonadConfig m, MonadIO m) => TagName -> m (Maybe Tag)
byName t = fmap (Tag t) . M.lookup t . unTags <$> loadTags

byPackage :: (MonadConfig m, MonadIO m) => PackageName -> m (Set TagName)
byPackage p = lookupByPackage p <$> loadTags

lookupByPackage :: PackageName -> Tags -> Set TagName
lookupByPackage p = S.fromList . M.keys . M.filter (p `S.member`) . unTags

toList :: (MonadConfig m, MonadIO m) => m [Tag]
toList = sortBy (comparing name) . map (uncurry Tag) . M.toList . unTags <$> loadTags

normalizeTagName :: Text -> TagName
normalizeTagName = TagName . T.replace " " "-" . T.replace "_" "-"

addTaggedPackage :: (MonadConfig m, MonadIO m) => PackageName -> TagName -> m ()
addTaggedPackage p t = writeTags . (<> singleton t p) =<< loadTags

removeTaggedPackage :: (MonadConfig m, MonadIO m) => PackageName -> TagName -> m Bool
removeTaggedPackage p t = do
  tags <- loadTags
  case M.lookup t . unTags $ tags of
    Nothing -> return False
    Just s -> do
      let modifier =
            if S.null (S.delete p s)
              then M.delete t
              else M.insert t (S.delete p s)
      writeTags . Tags . modifier . unTags $ tags
      return True
