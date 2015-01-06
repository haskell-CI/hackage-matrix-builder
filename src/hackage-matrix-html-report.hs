#!/usr/bin/env runghc

-- Copyright (C) 2015  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version
import           Text.ParserCombinators.ReadP          (readP_to_S, ReadP)
import           Text.XmlHtml


runReadP :: ReadP a -> String -> Maybe a
runReadP p s = listToMaybe [ x | (x,"") <- readP_to_S p s ]

parsePkgId :: Text -> (Text,Version)
parsePkgId s = (T.init n0, v)
  where
    Just v = runReadP parseVersion (T.unpack v0)
    (n0,v0) = T.breakOnEnd "-" s

thv :: (Text,Version) -> Node
thv (n,v) = Element "th" [] [ Element "a" [("href", hackUrl)] [TextNode vtxt]
                            , TextNode " "
                            , Element "a" [("href", hdiffUrl)] [TextNode "Δ"] ]
  where
    vtxt = T.pack (showVersion v)
    hdiffUrl = "http://hdiff.luite.com/cgit/" <> n <> "/commit?id=" <> vtxt
    hackUrl  = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/" <> n <> ".cabal/edit"

thgv :: (Text,Version) -> Node
thgv (_,v) = Element "th" [] [ TextNode vtxt ]
  where
    vtxt = T.pack (showVersion v)

main :: IO ()
main = do
    entries000 <- filter (not . isOther) . map toLine . T.lines <$> T.getContents

    let entries00 = split (dropFinalBlank $ keepDelimsR $ whenElt isStatus) entries000

    let entries0 = mapMaybe decodeBlock entries00

    let entries@((((pkgname,_),_),_):_) = [ ((parsePkgId x, parsePkgId y), z) | ((x,y), z) <- entries0 ]

    let vs = nub $ sort $ map (fst . fst) entries
    let gs = nub $ sort $ map (snd . fst) entries

    let doc = HtmlDocument UTF8 Nothing nodes
        nodes = [Element "html" [] [ Element "head" []
                                        [ Element "title" [] [TextNode $ "Report for " <> pkgname ]
                                        , Element "meta" [("charset","UTF-8")] []
                                        ]
                                   , Element "body" [] body ]]
        body = [tab]

        tab = Element "table" [("border","1")] rows

        hackurl = "https://hackage.haskell.org/package/" <> pkgname

        rows :: [Node]
        rows = Element "tr" [] (Element "th" [] [Element "a" [("href",hackurl)] [TextNode pkgname]] : map thgv gs)
               : [ Element "tr" [] (row v) | v <- vs ]

        row v = thv v : [ lup v g | g <- gs ]

        lup v g = case lookup (v,g) entries of
            Nothing -> Element "td" [("bgcolor","#000")] []
            Just (PassBuild c) -> Element "td" [("bgcolor","#0F0"),("title",c)] [TextNode "OK"]
            Just PassNoIp     -> Element "td" [("bgcolor","#070")] [TextNode "OK (no-ip)"]
            Just (FailBuild c) -> Element "td" [("bgcolor","#F00"),("title",c)] [TextNode "FAIL (pkg)"]
            Just (FailDepBuild c) -> Element "td" [("bgcolor","#700"),("title",c)] [TextNode "FAIL (deps)"]

    BS.putStrLn $ toByteString $ render doc

    return ()

  where

    decodeBlock blk = case last blk of
        LineStat "PASS-BUILD" l     -> Just (p l, PassBuild (c l))
        LineStat "PASS-NO-IP" l     -> Just (p l, PassNoIp)
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
data S = PassBuild !Text
       | PassNoIp
       | FailBuild !Text
       | FailDepBuild !Text
       deriving (Show,Eq,Ord)

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


