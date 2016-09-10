{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module IndexHelper (readIndexTimeMap, IndexTimeMap, indexTar, cabalDir, readPkgIndex, getPkgIndexTs, mapInternLst) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Text
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe

import           PkgId


cabalDir :: FilePath
cabalDir = unsafePerformIO (getAppUserDataDirectory "cabal")
{-# NOINLINE cabalDir #-}

indexTar :: FilePath
indexTar = cabalDir </> "packages" </> "hackage.haskell.org" </> "00-index.tar"

----------------------------------------------------------------------------

pkgIndex :: MVar (IndexHelper.IndexTimeMap, Integer)
pkgIndex = unsafePerformIO (newMVar (mempty, 0))
{-# NOINLINE pkgIndex #-}

-- | May re-read index if changed
readPkgIndex :: IO IndexHelper.IndexTimeMap
readPkgIndex = do
    sz <- getFileSize indexTar
    modifyMVar pkgIndex $ \(itm0, sz0) -> do
        if sz0 == sz
          then
            return ((itm0,sz0), itm0)
          else do
            putStrLn "re-reading pkg index"
            itm' <- readIndexTimeMap indexTar
            itm <- evaluate itm'
            return ((itm,sz),itm)

getPkgIndexTs :: IO Word
getPkgIndexTs = do
    itm <- readPkgIndex
    if IntMap.null itm
        then pure 0
        else do let !ts = fst (IntMap.findMax itm)
                pure (fromIntegral ts)

----------------------------------------------------------------------------

decodeEntry :: Tar.Entry -> Maybe (PkgN, Maybe Ver)
decodeEntry e
  | "/package.json" `isSuffixOf` fp = Nothing

  | ".cabal" `isSuffixOf` fp
  , (pn,pv) <- fn2pkgver fp
  = Just (pn, Just pv)

  | (pn', "preferred-versions") <- splitFileName fp
  = Just (T.pack pn', Nothing)

  | otherwise = error "decodeEntry: unexpected entry"
  where
    fp = Tar.entryPath e

decodeEntry' :: Tar.Entry -> Maybe (Int, (PkgN, Maybe Ver))
decodeEntry' e = fmap ((,) (fromIntegral $ Tar.entryTime e)) (decodeEntry e)


mapIntern :: Ord k => k -> Map k k -> (k,Map k k)
mapIntern k m = maybe (k,Map.insert k k m) (\k' -> (k',m)) (Map.lookup k m)

mapInternLst :: Ord k => [k] -> Map [k] [k] -> ([k],Map [k] [k])
mapInternLst ks m0 = go m0 (reverse ks) []
  where
    go m [] acc     = (acc, m)
    go m (c:cs) acc = go m' cs acc'
      where
        (acc',m') = mapIntern (c:acc) m

type IndexTimeMap = IntMap (Set PkgId)

makeTimeMap :: [Tar.Entry] -> IndexTimeMap
makeTimeMap = go IntMap.empty Set.empty 0 . internPkgIds . mapMaybe decodeEntry'
  where
    go :: (IntMap (Set PkgId)) -> (Set PkgId) -> Int -> [(Int, (PkgN, Maybe Ver))] -> IndexTimeMap
    go !acc  !_ !_  [] = acc
    go _    _ t0 ((t1,_):_) | t1 < t0 = error "makeTimeMap: time discontinuity"
    go acc0 m _  ((t1,(_,Nothing)):es) = go (IntMap.insert t1 m  acc0) m  t1 es
    go acc0 m _  ((t1,(n,Just v)):es)  = go (IntMap.insert t1 m' acc0) m' t1 es
      where
        !m' = Set.insert (PkgId n v) m

    internPkgIds :: [(Int, (PkgN, Maybe Ver))] -> [(Int, (PkgN, Maybe Ver))]
    internPkgIds = go2 mempty mempty
      where
        go2 _ _ [] = []
        go2 nc vc ((i,(n,mv)):es) = (i,(n',mv')) : go2 nc' vc' es
          where
            (n',nc') = mapIntern n nc
            (mv',vc') = case mv of
              Just (Ver v) -> first (Just . Ver) (mapInternLst v vc)
              Nothing      -> (Nothing, vc)


readIndexTimeMap :: FilePath -> IO IndexTimeMap
readIndexTimeMap fn = makeTimeMap <$> readIndex fn

-- main :: IO ()
-- main = do
--     idx <- readIndex
--     -- print (Map.size idx)

--     let tm = makeTimeMap idx

--     print (IntMap.size tm)
--     print (fst $ IntMap.findMin tm)
--     print (fst $ IntMap.findMax tm)

-- readIndex :: IO (Map PkgN (Map Ver BSL.ByteString))
readIndex :: FilePath -> IO [Tar.Entry]
readIndex idxtar = do
    es <- Tar.read <$> BSL.readFile idxtar
    -- return $! Map.fromListWith (flip mappend) [ (k1,Map.singleton k2 v) | (k1,(k2,v)) <- golst es ]
    return (Tar.foldEntries (:) [] (\_ -> error "readIndex") es)
  -- where
  --   gover2 (Tar.Next ent es)
  --       | takeExtension (Tar.entryPath ent) == ".cabal"
  --       , Tar.NormalFile e _ <- Tar.entryContent ent
  --       , (pn,pv) <- fn2pkgver (Tar.entryPath ent)   =  (pn,(pv,e)) : gover2 es
  --       | otherwise                                  =     gover2 es
  --   gover2 (Tar.Fail _) = error "gover2: Fail"
  --   gover2 Tar.Done        = []

  --   golst (Tar.Next ent es) = ent : golst es
  --   golst (Tar.Fail _)      = error "Tar.read: Fail"
  --   golst Tar.Done          = []

fn2pkgver :: FilePath -> (PkgN,Ver)
fn2pkgver fn = (T.pack n, readVer v0)
  where
    (n,'/':v0) = break (=='/') $ takeDirectory fn

    readVer :: String -> Ver
    readVer = fromMaybe (error "readVer") . simpleParse

