#!/usr/bin/env runghc

-- Copyright (C) 2015  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version
import           System.Environment
import           Text.ParserCombinators.ReadP          (readP_to_S, ReadP)
import           Text.XmlHtml

import           Common

main :: IO ()
main = do
    logfns <- fmap sort $ getArgs

    unless (all (isSuffixOf ".log") logfns) $
        fail "logfiles must have .log suffix"

    summaries <- mapM (fmap parseLogSummarise . T.readFile) logfns

    _ <- evaluate (force summaries)

    BS.putStrLn $ toByteString $ render $ summariesToHtml summaries


    return ()

summariesToHtml :: [(Text, [(Version, [(Status',Int)])])] -> Document
summariesToHtml summaries = HtmlDocument UTF8 Nothing doc
  where
    ghcVers = nub $ sort $ map fst $ concatMap snd summaries

    doc = [Element "html" [] [ Element "head" []
                                 [ Element "title" [] [TextNode $ "Build-reports index" ]
                                 , Element "meta" [("charset","UTF-8")] []
                                 ]
                               , Element "body" [] body ]]

    body = [tab]

    tab = Element "table" [("border","1")] rows

    rows = hrow : concatMap mkRow summaries

    hrow = Element "tr" [] (Element "th" [] [] : map thgv ghcVers)

    mkRow (pkgn, ss) =
        Element "tr" [] (Element "th" [("rowspan","4")] [Element "a" [("href",pkgn <> ".html")] [TextNode pkgn]]
                         : [ lup PassBuild' v ss | v <- ghcVers ])
        : [ Element "tr" [] ([ lup s' v ss | v <- ghcVers ]) | s' <- [PassNoIp', FailDepBuild', FailBuild'] ]

    lup s v ss
      | cnt == 0  = Element "td" [] []
      | otherwise = case s of
        PassBuild'    -> Element "td" [("bgcolor","#0F0"),("title","OK")]          [TextNode tcnt]
        PassNoIp'     -> Element "td" [("bgcolor","#070"),("title","OK (no-ip)")]  [TextNode tcnt]
        FailBuild'    -> Element "td" [("bgcolor","#F00"),("title","FAIL (pkg)")]  [TextNode tcnt]
        FailDepBuild' -> Element "td" [("bgcolor","#700"),("title","FAIL (deps)")] [TextNode tcnt]
      where
        tcnt = T.pack $ show cnt
        cnt = maybe 0 id $ lookup s $ maybe [] id (lookup v ss)

parseLogSummarise :: Text -> (Text, [(Version, [(Status',Int)])])
parseLogSummarise raw = (pkgn, entries)
  where
    entries = [ (ghcver, map (fmap length) . grouper truncStatus . Map.elems $ pkgvers)
              | (ghcver, pkgvers) <- Map.toAscList pkgmap ]

    (pkgn, _pkgvs, pkgmap) = parseLogMap raw
