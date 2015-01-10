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
summariesToHtml summaries = html5Doc doc
  where
    ghcVers = nub $ sort $ map fst $ concatMap snd summaries

    doc = [Element "html" [] [ Element "head" []
                                 [ Element "title" [] [TextNode $ "Build-reports index" ]
                                 , Element "meta" [("charset","UTF-8")] []
                                 , Element "link" [("rel","stylesheet"),("href","style.2.css")] []
                                 ]
                               , Element "body" [] body ]]

    body = [tab]

    tab = Element "table" [] rows

    rows = (header : map mkRow summaries) ++ [footer]

    header = Element "thead" [] [Element "tr" [] (Element "th" [] [] : map thgv ghcVers)]
    footer = Element "tfoot" [] [Element "tr" [] (Element "th" [] [] : map thgv ghcVers)]

    mkRow (pkgn, ss) = Element "tbody" [] $
                       [ mkTrTh PassBuild'
                       , mkTr   PassNoIp'
                       , mkTr   FailDepBuild'
                       , mkTr   FailBuild'
                       ]
      where
        th = Element "th" [("rowspan","4")] [Element "a" [("href",pkgn <> ".html")] [TextNode pkgn]]
        mkTrTh s' = Element "tr" [] (th : [ lup s' v ss | v <- ghcVers ])
        mkTr   s' = Element "tr" [] ([ lup s' v ss | v <- ghcVers ])

    lup s v ss
      | cnt == 0  = Element "td" [("class","sucell")] []
      | otherwise = case s of
        PassBuild'    -> mkTd "pass-build"     "OK"
        PassNoIp'     -> mkTd "pass-no-ip"     "OK (no-ip)"
        PassNoOp'     -> mkTd "pass-no-op"     "OK (boot)"
        FailBuild'    -> mkTd "fail-build"     "FAIL (pkg)"
        FailDepBuild' -> mkTd "fail-dep-build" "FAIL (deps)"
      where
        mkTd cls title = Element "td" [("class",cls <> " sucell"),("title",title)] [TextNode tcnt]

        tcnt = T.pack $ show cnt
        cnt = maybe 0 id $ lookup s $ maybe [] id (lookup v ss)

parseLogSummarise :: Text -> (Text, [(Version, [(Status',Int)])])
parseLogSummarise raw = (pkgn, entries)
  where
    entries = [ (ghcver, map (fmap length) . grouper truncStatus . Map.elems $ pkgvers)
              | (ghcver, pkgvers) <- Map.toAscList pkgmap ]

    (pkgn, _pkgvs, pkgmap, _, _) = parseLogMap raw
