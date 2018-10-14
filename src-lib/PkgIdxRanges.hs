{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module PkgIdxRanges
    ( IdxRanges
    , fromList
    , toList
    , member
    , null
    , isBounded
    , intersection
    , intersectionN
    ) where

import qualified Prelude
import           Prelude.Local                        hiding (normalise, null,
                                                       toList)

import           PkgId

import qualified Data.Set                             as Set
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Range
import           Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple.Types     as PGS

-- | Represents union of 'PkgIdxTs'-intervals
--
-- The upper bound may be unbounded.
newtype IdxRanges = IRA [(PkgIdxTs,Maybe PkgIdxTs)]
                  deriving (NFData,Eq)

instance Show IdxRanges where
    show (IRA rs) = "(" ++ intercalate " + " (map go rs) ++ ")"
        where
          go (PkgIdxTs l,Nothing)           = "["++show l++",+inf)"
          go (PkgIdxTs l,Just (PkgIdxTs r)) = "["++show l++","++show r++")"

-- | Serialises to @int4range[]@
instance ToField IdxRanges where
    toField (IRA rs) = Many [toField (PGS.PGArray rs'), Plain "::int4range[]" ]
      where
        rs' :: [PGRange Int]
        rs' = [ PGRange (Inclusive (unPkgIdxTs l)) (maybe PosInfinity (Exclusive . unPkgIdxTs) r) | (l,r) <- rs ]

-- | Deserialises from @int4range[]@
instance FromField IdxRanges where
    fromField f dat = go <$> fromField f dat
      where
        go :: PGS.PGArray (PGRange Int) -> IdxRanges
        go (PGS.PGArray rs) = IRA (map go2 rs)

        go2 :: PGRange Int -> (PkgIdxTs,Maybe PkgIdxTs)
        go2 (PGRange (Inclusive l) (Exclusive r)) = (PkgIdxTs l,Just $ PkgIdxTs r)
        go2 (PGRange (Inclusive l) PosInfinity)   = (PkgIdxTs l,Nothing)
        go2 (PGRange (Exclusive l) r) = go2 (PGRange (Inclusive (l+1)) r)
        go2 (PGRange l (Inclusive r)) = go2 (PGRange l (Exclusive (r+1)))
        go2 (PGRange PosInfinity NegInfinity) = (PkgIdxTs 0, Just (PkgIdxTs 0))
        go2 _ = error "FromField[IdxRanges]"

----------------------------------------------------------------------------
-- (de)construction

fromList :: [(PkgIdxTs,Maybe PkgIdxTs)] -> IdxRanges
fromList = normalise . IRA

toList :: IdxRanges -> [(PkgIdxTs,Maybe PkgIdxTs)]
toList (IRA rs) = rs

----------------------------------------------------------------------------
-- predicates

member :: PkgIdxTs -> IdxRanges -> Bool
member t (IRA rs) = any f rs
  where
    f (t0,Nothing) = t0 <= t
    f (t0,Just t1) = t0 <= t && t < t1

-- | Test whether 'IdxRanges' is the empty set
null :: IdxRanges -> Bool
null (IRA rs) = Prelude.null rs

-- | Test whether 'IdxRanges' represents an interval with non-infinite bounds.
isBounded :: IdxRanges -> Bool
isBounded (IRA rs)
  | Just (_,ub) <- last rs = isJust ub
  | otherwise              = True

----------------------------------------------------------------------------
-- operations

-- union monoid
instance Monoid IdxRanges where
    mempty = IRA []

    mappend (IRA []) y      = y
    mappend x (IRA [])      = x
    mappend (IRA x) (IRA y) = normalise (IRA (x++y))

intersection :: IdxRanges -> IdxRanges -> IdxRanges
intersection (IRA []) _ = mempty
intersection _ (IRA []) = mempty
intersection a0 b0 = normalise (IRA $ Set.toList res')
  where
    res' = Set.fromList (concatMap splitup1 as) `Set.intersection` Set.fromList (concatMap splitup1 bs)

    IRA as = normalise a0
    IRA bs = normalise b0

    splitps :: Set PkgIdxTs
    splitps = Set.fromList (concatMap f as) <> Set.fromList (concatMap f bs)

    f (x,Just y)  = [x,y]
    f (x,Nothing) = [x]

    splitup1 (l,mr) = zipWith (,) (l : lxs) (map Just lxs ++ [mr])
      where
        tmp = snd $ fmap Set.toList $ Set.split l splitps
        lxs = case mr of
                Nothing -> tmp
                Just r  -> takeWhile (<r) tmp

intersectionN :: NonEmpty IdxRanges -> IdxRanges
intersectionN = foldl1 intersection

----------------------------------------------------------------------------
-- internal helper

normalise :: IdxRanges -> IdxRanges
normalise (IRA []) = IRA []
normalise (IRA rs) = IRA (go (sort (filter ne rs)))
  where
    go []               = []
    go ((l1,Nothing):_) = [(l1,Nothing)]
    go [x@(_,Just _)]   = [x]
    go (lr1@(l1,Just r1):lr2@(l2,mr2):rest)
        | r1 < l2    = lr1 : go (lr2:rest) -- don't merge
        | otherwise  = case mr2 of -- merge
                         Nothing -> go ((l1,Nothing):rest)
                         Just r2 -> go ((l1,Just (max r1 r2)):rest)


    ne :: (PkgIdxTs,Maybe PkgIdxTs) -> Bool
    ne (ts1,Just ts2) = ts1 < ts2
    ne (_,Nothing)    = True
