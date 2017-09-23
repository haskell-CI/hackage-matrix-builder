{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

import           PkgId
import qualified PkgIdxTsSet           as PIS

import qualified Data.Set              as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
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

deriving instance Arbitrary PkgIdxTs
