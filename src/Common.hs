{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
    ( module Common
    , Version(..)
    ) where

-- Copyright (C) 2015  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version
import           Text.ParserCombinators.ReadP          (readP_to_S, ReadP)
import           Text.XmlHtml

type PkgName = Text

type PkgId = (PkgName,Version) -- (pkg-name, pkg-version)

runReadP :: ReadP a -> String -> Maybe a
runReadP p s = listToMaybe [ x | (x,"") <- readP_to_S p s ]

parsePkgId' :: Text -> PkgId
parsePkgId' s = fromMaybe (error $ "parsePkgId " ++ show s) . parsePkgId $ s

parsePkgId :: Text -> Maybe PkgId
parsePkgId s = do
    v <- parseVer v0
    guard (T.length n0 > 1)

    return (T.init n0, v)
  where
    (n0,v0) = T.breakOnEnd "-" s

parseVer :: Text -> Maybe Version
parseVer = runReadP parseVersion . T.unpack

parseVer' :: Text -> Version
parseVer' s = fromMaybe (error $ "parseVer " ++ show s) . parseVer $ s

dispVer :: Version -> Text
dispVer = T.pack . showVersion

dispPkgId :: PkgId -> Text
dispPkgId (n,v) = n <> "-" <> dispVer v

html5Doc :: [Node] -> Document
html5Doc = HtmlDocument UTF8 (Just $ DocType "html" NoExternalID NoInternalSubset)

-- | Format GHC version as TH cell
thgv :: Version -> Node
thgv v = Element "th" [] [ TextNode vtxt ]
  where
    vtxt = T.pack (showVersion v)


-- (pkgname, [pkgver], ghcver ~> pkgver ~> status)
parseLogMap :: Text -> (Text, [Version], Map Version (Map Version Status), Set.Set Version, Map Version Int)
parseLogMap raw = force (pkgname, vs, mapping, Set.fromList norls, Map.fromList xrevs)
  where
    (pkgname, entries,norls,xrevs) = parseLog raw
    vs = nub $ sort $ map (fst . fst) entries

    mapping = Map.fromList [ (k,Map.fromList [ (k',s) | ((k',_),s) <- es ])
                           | (k, es) <- grouper (snd . fst) entries ]

grouper :: Ord a => (b -> a) -> [b] -> [(a,[b])]
grouper sel ys = [ (sel $ head xs, xs)
                 | xs <- groupBy ((==) `on` sel) . sortBy (comparing sel) $ ys ]

parseLog :: Text -> (Text, [((Version, Version), Status)], [Version], [(Version,Int)]) -- (pkgname, [((pkgver, ghcver), stat)])
parseLog raw = force $ (pkgname, sort $ entries, sort noRls, sort xrevs)
  where
    pkgname    = fst . parsePkgId' . fst . fst . head $ entries0
    entries    = [ ((snd $ parsePkgId' x, parseGhcPkgId y), z) | ((x,y), z) <- entries0 ]

    entries0   = mapMaybe decodeBlock entries00
    entries00  = split (dropFinalBlank $ keepDelimsR $ whenElt isStatus) entries000
    entries000 = filter (not . isOther) . map toLine . T.lines $ raw

    noRls = [ snd (parsePkgId' l) | LineStat "NO-RLS" l <- entries000 ]

    xrevs = [ (snd (parsePkgId' p), read (T.unpack x)) | LineStat "XREV" l <- entries000, let (p:x:_) = T.words l ]

    parseGhcPkgId s = let ("GHC",v) = parsePkgId' s in v

    decodeBlock blk = case last blk of
        LineStat "PASS-BUILD" l     -> Just (p l, PassBuild (c l))
        LineStat "PASS-NO-IP" l     -> Just (p l, PassNoIp)
        LineStat "PASS-NO-OP" l     -> Just (p l, PassNoOp)
        LineStat "FAIL-BUILD" l     -> Just (p l, FailBuild (c l))
        LineStat "FAIL-DEP-BUILD" l -> Just (p l, FailDepBuild (c l))
        LineStat _ _                -> Nothing
        LineOut _ _ -> error "decodeBlock"
        LineOther _ -> error "decodeBlock"
      where
        p l = case T.words l of
            (pkg:ghc:_) -> (pkg,ghc)
            _           -> error "decodeBlock:p"

        c l = case T.words l of
            (_:_:deps) -> "installed build-deps:\n  " <> T.intercalate " " deps <> errout
            _          -> error "decodeBlock:c"

        errout = case [ x | LineOut StdErr x <- blk ] of
            [] -> ""
            xs -> "\n\nSTDERR:" <> T.unlines xs

-- | Represents build outcome status with associated meta-info
data Status
    = PassBuild !Text
    | PassNoIp
    | PassNoOp
    | FailBuild !Text
    | FailDepBuild !Text
    deriving (Show,Eq,Ord)

data Status' = PassBuild' | PassNoIp' | PassNoOp' | FailBuild' | FailDepBuild'
             deriving (Show,Eq,Ord)

truncStatus :: Status -> Status'
truncStatus s = case s of
    PassBuild _    -> PassBuild'
    PassNoIp       -> PassNoIp'
    PassNoOp       -> PassNoOp'
    FailBuild _    -> FailBuild'
    FailDepBuild _ -> FailDepBuild'

instance NFData Status where rnf !_ = ()
instance NFData Status' where rnf !_ = ()

-- | Represents single line in build-log
data Line = LineStat !Text !Text
          | LineOut  !StdOutErr !Text
          | LineOther !Text
          deriving (Show,Eq,Ord)

data StdOutErr = StdOut | StdErr
               deriving (Show,Eq,Ord)

isStatus :: Line -> Bool
isStatus (LineStat _ _) = True
isStatus _              = False

isOther :: Line -> Bool
isOther (LineOther _)   = True
isOther _               = False

-- | Converts single 'Text'-line to pre-parsed 'Line'
toLine :: Text -> Line
toLine l
  | T.isPrefixOf "STATUS:" l = let (k,l') = fmap T.tail $ T.breakOn ":" $ T.takeWhile (/='#') $ T.drop 7 l in LineStat k l'
  | T.isPrefixOf "STDERR:" l = LineOut StdErr (T.drop 7 l)
  | T.isPrefixOf "STDOUT:" l = LineOut StdOut (T.drop 7 l)
  | otherwise = LineOther l
  where
