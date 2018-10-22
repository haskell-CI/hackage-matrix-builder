{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker.PkgIndex
    ( getPkgIndexTs
    , pkgIndexTs
    , readPkgIndex
    , pkgIndexTsMember
    , pkgIndexMember
    , PkgIndex
    ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe    (unsafePerformIO)

import           IndexHelper         (PkgIdxTuple (..), indexTar,
                                      readIndexTuples)
import           Log
import           PkgId
import           Prelude.Local

import           Debug.Trace

data PkgIndex = PkgIndex !(Vector PkgIdxTsId) !(VU.Vector PkgIdxTs)

instance NFData PkgIndex where
  rnf (PkgIndex x !_) = rnf x

pkgIndex :: MVar (PkgIndex, Integer)
pkgIndex = unsafePerformIO (newMVar (PkgIndex mempty mempty, 0))
{-# NOINLINE pkgIndex #-}

pkgIndexTs :: PkgIndex -> PkgIdxTs
pkgIndexTs (PkgIndex v1 v2) = max t1 t2
  where
    t1 = if V.null v1  then PkgIdxTs 0 else (case V.last v1 of PkgIdxTsId x _ _ -> x)
    t2 = if VU.null v2 then PkgIdxTs 0 else VU.last v2

getPkgIndexTs :: IO PkgIdxTs
getPkgIndexTs = pkgIndexTs <$> readPkgIndex

pkgIndexTsMember :: PkgIdxTs -> PkgIndex -> Bool
pkgIndexTsMember t (PkgIndex v1 v2)
  | VU.elem t v2 = True
  | otherwise = V.any (\(PkgIdxTsId t0 _ _) -> t == t0) v1

pkgIndexMember :: PkgIdxTs -> PkgId -> PkgIndex -> Bool
pkgIndexMember t pid (PkgIndex v1 _) = V.any (pkgIdxTsIsPkgId pid) (V.takeWhile ((<= t) . pkgIdxTs) v1)

readPkgIndex :: IO PkgIndex
readPkgIndex = do
    sz <- getFileSize indexTar
    modifyMVar pkgIndex $ \(itm0, sz0) -> do
        if sz0 == sz
          then
            return ((itm0,sz0), itm0)
          else do
            logInfo "re-reading pkg index"
            itm <- readPkgIndex0 indexTar
            logInfo "pkg index load complete"
            return ((itm,sz), itm)


readPkgIndex0 :: FilePath -> IO PkgIndex
readPkgIndex0 fn = do
    xs0 <- readIndexTuples fn
    let (vec1, vec2) = partitionEithers (map f xs0)

    evaluate $ force (PkgIndex (V.fromList (mono1 vec1)) (VU.fromList (uniq2 vec2)))

  where
    f (PkgIdxTuple { pitTime = t, pitVer = Nothing }) = Right t
    f (PkgIdxTuple { pitTime = t, pitName = n, pitVer = Just v, pitRev = rv })
       | rv > 0 = Right t
       | otherwise  = Left $! PkgIdxTsId t n v

    mono1 (x1@(PkgIdxTsId t1 _ _):x2@(PkgIdxTsId t2 _ _):xs)
      | t1 <= t2  = x1 : mono1 (x2:xs)
      | otherwise = error "readPkgIndex.mono1"
    mono1 xs = xs

    uniq2 (x1:x2:xs) = case compare x1 x2 of
                         LT -> x1 : uniq2 (x2:xs)
                         EQ -> uniq2 (x2:xs)
                         GT -> error "readPkgIndex.uniq2"
    uniq2 xs = xs

pkgIdxTs :: PkgIdxTsId -> PkgIdxTs
pkgIdxTs (PkgIdxTsId t _ _) = t

pkgIdxTsIsPkgId :: PkgId -> PkgIdxTsId -> Bool
pkgIdxTsIsPkgId (PkgId pn pv) (PkgIdxTsId _ pn' pv') = (pn == pn') && (pv == pv')


data PkgIdxTsId = PkgIdxTsId !PkgIdxTs !PkgN !Ver
                deriving (Show,Ord,Eq)

instance NFData PkgIdxTsId where
  rnf (PkgIdxTsId _ _ v) = rnf v

