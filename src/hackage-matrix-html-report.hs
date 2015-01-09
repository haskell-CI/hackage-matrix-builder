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
import qualified Data.Map.Strict as Map

import           Common

main :: IO ()
main = do
    doc <- fmap parseToHtmlReport T.getContents

    BS.putStrLn $ toByteString $ render doc

    return ()

parseToHtmlReport :: Text -> Document
parseToHtmlReport t = doc
  where
    (pkgname, vs, mapping) = parseLogMap t

    -- GHC versions found in matrix
    gvs = Map.keys mapping

    doc = HtmlDocument UTF8 Nothing nodes
    nodes = [Element "html" [] [ Element "head" []
                                    [ Element "title" [] [TextNode $ "Report for " <> pkgname ]
                                    , Element "meta" [("charset","UTF-8")] []
                                    ]
                               , Element "body" [] body ]]
    body = [ Element "p" [] [ Element "a" [("href","0INDEX.html")] [Element "small" [] [TextNode "back to index"]] ]
           , tab
           , Element "h4" [] [ TextNode "Legend" ]
           , legendTab
           ]

    tab = Element "table" [("border","1")] rows

    hackurl = "https://hackage.haskell.org/package/" <> pkgname

    rows :: [Node]
    rows = Element "tr" [] (Element "th" [] [Element "a" [("href",hackurl)] [TextNode pkgname]] : map thgv gvs)
           : [ Element "tr" [] (row v) | v <- vs ]

    row v = thv v : [ lup v g | g <- gvs ]

    lup v g = case Map.lookup v $ Map.findWithDefault Map.empty g mapping of
        Nothing               -> Element "td" [("bgcolor","#000")] []
        Just (PassBuild c)    -> Element "td" [("bgcolor","#0F0"),("title",c)] [TextNode "OK"]
        Just PassNoIp         -> Element "td" [("bgcolor","#070")]             [TextNode "OK (no-ip)"]
        Just (FailBuild c)    -> Element "td" [("bgcolor","#F00"),("title",c)] [TextNode "FAIL (pkg)"]
        Just (FailDepBuild c) -> Element "td" [("bgcolor","#700"),("title",c)] [TextNode "FAIL (deps)"]


    legendTab = Element "table" [("border","1")]
      [ Element "tr" [] [ Element "td" [("bgcolor","#000")] []
                        , Element "td" [] [TextNode "test-result missing"]
                        ]
      , Element "tr" [] [ Element "td" [("bgcolor","#0F0")] [TextNode "OK"]
                        , Element "td" [] [TextNode "package build succesful"]
                        ]
      , Element "tr" [] [ Element "td" [("bgcolor","#070")] [TextNode "OK (no-ip)"]
                        , Element "td" [] [TextNode "no install-plan found"]
                        ]
      , Element "tr" [] [ Element "td" [("bgcolor","#F00")] [TextNode "FAIL (pkg)"]
                        , Element "td" [] [TextNode "package failed to build"]
                        ]
      , Element "tr" [] [ Element "td" [("bgcolor","#700")] [TextNode "FAIL (deps)"]
                        , Element "td" [] [TextNode "package dependencies failed to build"]
                        ]
      ]

    thv :: Version -> Node
    thv v = Element "th" [] [ Element "a" [("href", hackUrl)] [TextNode vtxt]
                                , TextNode " "
                                , Element "a" [("href", hdiffUrl)] [TextNode "Δ"] ]
      where
        n = pkgname
        vtxt = T.pack (showVersion v)
        hdiffUrl = "http://hdiff.luite.com/cgit/" <> n <> "/commit?id=" <> vtxt
        hackUrl  = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/" <> n <> ".cabal/edit"
