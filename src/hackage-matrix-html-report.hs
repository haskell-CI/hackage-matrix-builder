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
import qualified Data.Set as Set

import           Common

main :: IO ()
main = do
    doc <- fmap parseToHtmlReport T.getContents

    BS.putStrLn $ toByteString $ render doc

    return ()

parseToHtmlReport :: Text -> Document
parseToHtmlReport t = html5Doc doc
  where
    (pkgname, vs, mapping, norls, xrevs) = parseLogMap t

    -- GHC versions found in matrix
    gvs = Map.keys mapping

    lastMajVs = Set.fromList $ map (last . snd) (grouper majorVer vs)
    lastMinVs = Set.fromList $ map (last . snd) (grouper minorVer vs)

    doc = [Element "html" [] [ Element "head" []
                                    [ Element "title" [] [TextNode $ "Report for " <> pkgname ]
                                    , Element "meta" [("charset","UTF-8")] []
                                    , Element "link" [("rel","stylesheet"),("href","style.2.css")] []
                                    ]
                               , Element "body" [] body ]]
    body = [ Element "p" [] [ Element "a" [("href","0INDEX.html")] [Element "small" [] [TextNode "back to index"]] ]
           , tab
           , Element "h4" [] [ TextNode "Legend" ]
           , legendTab
           -- , Element "pre" [] [ TextNode $ T.pack $ show lastMajVs ]
           ]

    tab = Element "table" [] rows

    hackurl = "https://hackage.haskell.org/package/" <> pkgname

    rows :: [Node]
    rows = [ Element "thead" [] [Element "tr" []
                                 (Element "th" [] [Element "a" [("href",hackurl)] [TextNode pkgname]]
                                  : map thgv gvs)]
           , Element "tbody" [] [ Element "tr" [] (row v) | v <- vs ]
           , Element "tfoot" [] [Element "tr" [] (Element "th" [("class","empty-lb")] [] : map thgv gvs)]
           ]

    row v = thv v : [ lup v g | g <- gvs ]

    lup v g = case Map.lookup v $ Map.findWithDefault Map.empty g mapping of
        Nothing               -> mkTd "fail-unknown"   "" ""
        Just (PassBuild c)    -> mkTd "pass-build"     c  "OK"
        Just PassNoIp
          | v `Set.member` norls -> mkTd "fail-unknown"   "unreleased" ""
          | otherwise            -> mkTd "pass-no-ip"     "" "OK (no-ip)"
        Just PassNoOp         -> mkTd "pass-no-op"     "" "OK (boot)"
        Just (FailBuild c)    -> mkTd "fail-build"     c  "FAIL (pkg)"
        Just (FailDepBuild c) -> mkTd "fail-dep-build" c  "FAIL (deps)"
      where
        xcls | v `Set.member` lastMajVs = " stcell lastmaj"
             | v `Set.member` lastMinVs = " stcell lastmin"
             | otherwise                = " stcell"

        mkTd cls title lab = Element "td"
                                     (("class",cls <> xcls)
                                       : [("title",title) | title /= ""])
                                     [TextNode lab | lab /= "" ]

    legendTab = Element "table" [] $
      [ Element "tr" [] [ Element "td" [("class",cls <> " stcell")] [ TextNode l | l /= "" ]
                        , Element "td" [("style","text-align:left; padding-left: 5px")] [TextNode cmt] ]
      | (cls,l,cmt) <- [ ("pass-build",     "OK",          "package build succesful")
                       , ("pass-no-op",     "OK (boot)",   "pre-installed version")
                       , ("pass-no-ip",     "OK (no-ip)",  "no install-plan found")
                       , ("fail-build",     "FAIL (pkg)",  "package failed to build")
                       , ("fail-dep-build", "FAIL (deps)", "package dependencies failed to build")
                       , ("fail-unknown",   "",            "test-result missing")
                       ]
      ]

    thv :: Version -> Node
    thv v = Element "th" [("class","pkgv" <> xcls)]
                   ([ Element "a" [("href", hdiffUrl),("title","diff to previous version")] [TextNode "Δ"]
                    , TextNode " "
                    , Element "a" [("href", hackUrl),("title","edit cabal file")] [TextNode vtxt] ]
                    ++ xrevnode
                   )
      where
        xcls | v `Set.member` lastMajVs = " lastmaj"
             | v `Set.member` lastMinVs = " lastmin"
             | otherwise                = ""

        xrev = Map.findWithDefault 0 v xrevs
        xrevt = "(" <> (T.pack $ show xrev) <> ")"

        xrevnode = [ Element "sup" [] [Element "a" [("class","xrev"),("href",revLogUrl),("title","revision log")]
                                       [TextNode xrevt]]
                   | xrev /= 0 ]

        n = pkgname
        vtxt = T.pack (showVersion v)
        hdiffUrl = "http://hdiff.luite.com/cgit/" <> n <> "/commit?id=" <> vtxt
        hackUrl  = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/" <> n <> ".cabal/edit"
        revLogUrl = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/revisions"

