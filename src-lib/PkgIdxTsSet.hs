{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PkgIdxTsSet
    ( PkgIdxTsSet
    , null
    , size
    , empty

    , fromSet
    , fromDistinctAscList
    , toAscList
    , toSet

    , member
    , lookupIndex
    , lookupIndexLE
    , lookupIndexGE

    , findMax
    , findMin

    , take
    , drop
    ) where

import           Prelude.Local       hiding (drop, null, take)
import           Prelude             ()

import qualified Data.Set            as Set
import qualified Data.Vector.Unboxed as VU
import           PkgId

newtype PkgIdxTsSet = PkgIdxTsSet (VU.Vector PkgIdxTs)
                    deriving (Show,NFData,ToJSON,FromJSON)

instance ToSchema PkgIdxTsSet where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy ([PkgIdxTs]))


fromSet :: Set.Set PkgIdxTs -> PkgIdxTsSet
fromSet = PkgIdxTsSet . VU.fromList . Set.toAscList

fromDistinctAscList :: [PkgIdxTs] -> Maybe PkgIdxTsSet
fromDistinctAscList [] = Just (PkgIdxTsSet VU.empty)
fromDistinctAscList lst@(x0:xs) = do
    len <- go 1 x0 xs
    pure $! PkgIdxTsSet (VU.fromListN len lst)
  where
    go !l _  []                  = Just l
    go !l y0 (y1:ys) | y0 < y1   = go (l+1) y1 ys
                     | otherwise = Nothing

toAscList :: PkgIdxTsSet -> [PkgIdxTs]
toAscList = coerce VU.toList

toSet :: PkgIdxTsSet -> Set.Set PkgIdxTs
toSet = Set.fromDistinctAscList . toAscList

-- finds largest entry smaller than/equal 'x'; except when there isn't
lookupIndexNearL :: PkgIdxTs -> PkgIdxTsSet -> Maybe (Int,PkgIdxTs)
lookupIndexNearL x s@(PkgIdxTsSet vec)
  | null s = Nothing
  | x <= lv0   = Just (li0, lv0)
  | uv0 <= x   = Just (ui0, uv0)
  -- invariant: lv0 < v < uv0   && li < ui
  | otherwise = go li0 ui0
  where
    li0 = 0
    ui0 = VU.length vec - 1

    lv0, uv0 :: PkgIdxTs
    lv0 = VU.head vec
    uv0 = VU.last vec

    go :: Int -> Int -> Maybe (Int,PkgIdxTs)
    go li ui -- invariant: li < ui
      | mi == li = Just (mi,mv) -- closest match
      | mv ==  x = Just (mi,mv) -- exact match
      | x  <  mv = go li mi -- go left
      | mv <   x = go mi ui -- go right
      | otherwise = error "the impossible happened"
      where
        mi = mid li ui
        mv :: PkgIdxTs
        mv = vec VU.! mi

    mid :: Int -> Int -> Int
    mid li ui = li + quot (ui-li) 2

-- finds smallest entry bigger than/equal 'x'; except when there isn't, then the max entry is returned
lookupIndexNearR :: PkgIdxTs -> PkgIdxTsSet -> Maybe (Int,PkgIdxTs)
lookupIndexNearR x s@(PkgIdxTsSet vec)
  | null s     = Nothing
  | x <= lv0   = Just (li0, lv0)
  | uv0 <= x   = Just (ui0, uv0)
  -- invariant: lv0 < v < uv0   && li < ui
  | otherwise = go li0 ui0
  where
    li0 = 0
    ui0 = VU.length vec - 1

    lv0, uv0 :: PkgIdxTs
    lv0 = VU.head vec
    uv0 = VU.last vec

    go :: Int -> Int -> Maybe (Int,PkgIdxTs)
    go li ui -- invariant: li < ui
      | mi == ui = Just (mi,mv) -- close match
      | otherwise = case compare x mv of
                      LT -> go li mi -- go left
                      EQ -> Just (mi,mv) -- exact match
                      GT -> go mi ui -- go right
      where
        mi = mid li ui
        mv :: PkgIdxTs
        mv = vec VU.! mi

    mid :: Int -> Int -> Int
    mid li ui = ui - quot (ui-li) 2

-- | Is 'PkgIdxTs' contained in 'PkgIdxTsSet'?
member :: PkgIdxTs -> PkgIdxTsSet -> Bool
member x = maybe False ((== x) . snd) . lookupIndexNearL x

-- | Lookup the index of an element, which is its zero-based index in
-- the sorted sequence of elements. The index is a number from 0 up
-- to, but not including, the size of the set.
lookupIndex :: PkgIdxTs -> PkgIdxTsSet -> Maybe Int
lookupIndex x s = case lookupIndexNearL x s of
                    Nothing                 -> Nothing
                    Just (i,x') | x' == x   -> Just i
                                | otherwise -> Nothing

-- | Find largest element smaller or equal to the given one.
lookupIndexLE :: PkgIdxTs -> PkgIdxTsSet -> Maybe (Int,PkgIdxTs)
lookupIndexLE x s = case lookupIndexNearL x s of
                      Nothing                 -> Nothing
                      Just (i,x') | x' <= x   -> Just (i,x')
                                  | otherwise -> Nothing

-- | Find largest element smaller or equal to the given one.
lookupIndexGE :: PkgIdxTs -> PkgIdxTsSet -> Maybe (Int,PkgIdxTs)
lookupIndexGE x s = case lookupIndexNearR x s of
                      Nothing                 -> Nothing
                      Just (i,x') | x' >= x   -> Just (i,x')
                                  | otherwise -> Nothing

findMin, findMax :: PkgIdxTsSet -> Maybe PkgIdxTs
findMin s | null s    = Nothing
          | otherwise = Just $! coerce VU.head s

findMax s | null s    = Nothing
          | otherwise = Just $! coerce VU.last s

take :: Int -> PkgIdxTsSet -> PkgIdxTsSet
take = coerce VU.take

drop :: Int -> PkgIdxTsSet -> PkgIdxTsSet
drop = coerce VU.drop

size :: PkgIdxTsSet -> Int
size = coerce VU.length

empty :: PkgIdxTsSet
empty = coerce VU.empty

null :: PkgIdxTsSet -> Bool
null = coerce VU.null
