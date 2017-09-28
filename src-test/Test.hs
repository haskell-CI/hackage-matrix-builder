{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

import           PkgId
import qualified PkgIdxTsSet           as PIS
import qualified PkgIdxRanges          as IR

import qualified Data.Set              as Set
import           Data.Word

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps2,qcProps1]


qcProps1 = testGroup "PkgIdxTsSet"
  [ QC.testProperty "Eq" $
      \x -> (x :: Int) == x

  , QC.testProperty "toSet . fromSet" $
      \s -> (PIS.toSet . PIS.fromSet) s == s

  , QC.testProperty "member" $
      \s -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
            in and [ PIS.member (PkgIdxTs x) s' == Set.member (PkgIdxTs x) s
                   | x <- [ lx - 50 .. lx + 50 ] ++ [ ux - 50 .. ux + 50 ]
                   ]

  , QC.testProperty "member2" $
      \s x -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
                x' = PkgIdxTs (wrapIntoRange (lx-2, ux+2) x)
            in PIS.member x' s' == Set.member x' s

  , QC.testProperty "lookupIndex" $
      \s -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
            in and [ PIS.lookupIndex (PkgIdxTs x) s' == Set.lookupIndex (PkgIdxTs x) s
                   | x <- [ lx - 50 .. lx + 50 ] ++ [ ux - 50 .. ux + 50 ]
                   ]

  , QC.testProperty "lookupIndex2" $
      \s x -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
                x' = PkgIdxTs (wrapIntoRange (lx-2, ux+2) x)
            in PIS.lookupIndex x' s' == Set.lookupIndex x' s

  , QC.testProperty "lookupIndexLE" $
      \s x -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
                x' = PkgIdxTs (wrapIntoRange (lx-2, ux+2) x)
            in (fmap snd (PIS.lookupIndexLE x' s') == Set.lookupLE x' s) &&
               (maybe True (\(i,v) -> Set.elemAt i s == v) (PIS.lookupIndexLE x' s'))

  , QC.testProperty "lookupIndexGE" $
      \s x -> (not (Set.null s)) QC.==>
            let PkgIdxTs lx = Set.findMin s
                PkgIdxTs ux = Set.findMin s
                s' = PIS.fromSet s
                x' = PkgIdxTs (wrapIntoRange (lx-2, ux+2) x)
            in (fmap snd (PIS.lookupIndexGE x' s') == Set.lookupGE x' s) &&
               (maybe True (\(i,v) -> Set.elemAt i s == v) (PIS.lookupIndexGE x' s'))

  ]

wrapIntoRange :: (Int,Int) -> Int -> Int
wrapIntoRange (l,u) x = l + ((x-l) `mod` (u-l))

----------------------------------------------------------------------------

qcProps2 = testGroup "PkgIdxRanges"
  [ QC.testProperty "eq1" $
      \rs -> IR.fromList rs == IR.fromList (reverse rs)

  , QC.testProperty "eq2" $
      \rs -> irFromList8 rs == irFromList8 (reverse rs)

  , QC.testProperty "eq3" $
      \rs1 rs2 -> let rs1' = irFromList8 rs1
                      rs2' = irFromList8 rs2
                  in (irToSet rs1' == irToSet rs2') == (rs1' == rs2')

  , QC.testProperty "union" $
      \rs1 rs2 -> let rs1' = irFromList8 rs1
                      rs2' = irFromList8 rs2
                      rs12' = mappend rs1' rs2'
                  in (irToSet rs1' `mappend` irToSet rs2') == irToSet rs12'

  , QC.testProperty "intersection" $
      \rs1 rs2 -> let rs1' = irFromList8 rs1
                      rs2' = irFromList8 rs2
                      rs12' = IR.intersection rs1' rs2'
                  in (irToSet rs1' `Set.intersection` irToSet rs2') == irToSet rs12'

  ]

irFromList8 :: [(Word8, Word8)] -> IR.IdxRanges
irFromList8 = IR.fromList . map f
  where
    f :: (Word8,Word8) -> (PkgIdxTs,Maybe PkgIdxTs)
    f (l,0) = (PkgIdxTs (fromIntegral l), Nothing)
    f (l,b) = (PkgIdxTs (fromIntegral (min l b)), Just (PkgIdxTs (fromIntegral (max l b))))

irToSet :: IR.IdxRanges -> Set.Set PkgIdxTs
irToSet ir = Set.fromList [ PkgIdxTs j | j <- [ (-2) .. 260 ], IR.member (PkgIdxTs j) ir ]


deriving instance Arbitrary PkgIdxTs
