{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module BuildReport (ReportData(..), genHtmlReport, docToBS) where

import           Data.Function
import           Data.List
-- import           Data.List.Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import           Blaze.ByteString.Builder
import           Control.Lens
import           Text.XmlHtml

import           BuildTypes

data ReportData = ReportData
    { rdPkgName     :: PkgName
    , rdVersions    :: Map PkgVer ( PkgRev, Bool )
    , rdGVersions   :: Map GhcVer ( PkgVer
                                  , Map PkgVer Status
                                  , Map PkgVerPfx (Maybe PkgVer)
                                  )
    } deriving (Show,Read,Generic,NFData)


{-
-- Sorted by severity
data Status' = PassBuild' | PassNoOp' | PassNoIp' | FailDepBuild' | FailBuild'
             deriving (Show,Eq,Ord)

truncStatus :: Status -> Status'
truncStatus s = case s of
    PassBuild _    -> PassBuild'
    PassNoIp       -> PassNoIp'
    PassNoOp       -> PassNoOp'
    FailBuild _    -> FailBuild'
    FailDepBuild _ -> FailDepBuild'
-}

tshow :: Show a => a -> Text
tshow = T.pack . show

dispPkgVer :: PkgVer -> Text
dispPkgVer = T.pack . showPkgVer

html5Doc :: [Node] -> Document
html5Doc = HtmlDocument UTF8 (Just $ DocType "html" NoExternalID NoInternalSubset)

-- | Format GHC version as TH cell
thgv :: PkgVer -> Node
thgv (PkgVer v) = Element "th" [] $ case v of
        [mj1]            -> [ TextNode (dispVer' v) ]
        [mj1,mj2]        -> [ TextNode (dispVer' v) ]
        [mj1,mj2,mi]     -> [ TextNode (dispVer' [mj1,mj2])
                            , Element "small" [] [TextNode ("." <> dispVer' [mi])]
                            ]
        (mj1:mj2:mi:ps)  -> [ TextNode (dispVer' [mj1,mj2])
                            , Element "small" [] [TextNode ("." <> dispVer' [mi])]
                            , Element "small" [] [Element "small" [] [TextNode ("." <> dispVer' ps)]]
                            ]
  where
    dispVer' = dispPkgVer . PkgVer

grouper :: Ord a => (b -> a) -> [b] -> [(a,[b])]
grouper sel ys = [ (sel $ head xs, xs)
                 | xs <- groupBy ((==) `on` sel) . sortBy (comparing sel) $ ys ]

docToBS :: Document -> ByteString
docToBS = toByteString . render

genHtmlReport :: Either FilePath Text -> ReportData -> Document
genHtmlReport css ReportData {..} = html5Doc doc
  where
    pkgname = T.pack (unPkgName rdPkgName)
    vs      = Map.keys rdVersions
    norls   = Set.empty -- FIXME
    xrevs   = fmap fst rdVersions

    -- GHC versions found in matrix
    gvs = Map.keys rdGVersions
    thgv' gv = thgv $ fromMaybe (error "thgv") $ rdGVersions ^? ix gv._1

    lastMajVs = Set.fromList $ map (last . snd) (grouper majorVer vs)
    lastMinVs = Set.fromList $ map (last . snd) (grouper majMinVer vs)

    doc = [Element "html" [] [ Element "head" []
                                    [ Element "title" [] [TextNode $ "Report for " <> pkgname ]
                                    , Element "meta" [("charset","UTF-8")] []
                                    , case css of
                                           Left fp -> Element "link" [("rel","stylesheet"),("href","../style.css")] []
                                           Right cssdat -> Element "style" [] [TextNode cssdat]
                                    ]
                               , Element "body" [] body ]]
    body = [ Element "p" [] [ Element "a" [("href","0INDEX.html")] [Element "small" [] [TextNode "back to index"]] ]

           , Element "h4" [("id","default")] [ TextNode "Solver Matrix (constraint-less)" ]
           , solverTab1

           , Element "h4" [("id","by-major-ver")] [ TextNode "Solver Matrix (constrained by major-version)" ]
           , solverTab

           , Element "h4" [("id","detailed")] [ TextNode "Solver Matrix (constrained by single version)" ]
           , tab

           , Element "h4" [("id","legend")] [ TextNode "Legend" ]
           , legendTab
           ]

    solverTab1 = Element "table" [] solverRows1
    solverTab = Element "table" [] solverRows

    majVsPfxs = [ [a,b] | (a,b) <- nub . sort . map majorVer $ vs ]

    dispVPfx [] = "*"
    dispVPfx vs = dispPkgVer $ PkgVer vs

    solverRows1 =
        [ Element "thead" [] [ Element "tr" [] (thPkgName : map thgv' gvs)]
        , Element "tbody" []
          [ Element "tr" [("class","solver-row")]
            (Element "th" [("class","pkgv lastmaj")] [TextNode $ "*" ] :
             [ lup' gv [] | gv <- gvs ])
          ]
        ]

    solverRows =
        [ Element "thead" [] [ Element "tr" [] (thPkgName : map thgv' gvs)]
        , Element "tbody" []
          [ Element "tr" [("class","solver-row")]
            (Element "th" [("class","pkgv lastmaj")] [TextNode $ dispVPfx mjv ] :
             [ lup' gv mjv | gv <- gvs ])
          | mjv <- majVsPfxs ]
        ]

    lup' gv mjv = case rdGVersions ^? ix gv._3.ix mjv of
        Nothing -> Element "td" [("class","lastmaj stcell")] [TextNode "???"]
        Just Nothing -> Element "td" [("class","lastmaj stcell pass-no-ip")] [TextNode "∅"]
        Just (Just v) -> case lup v gv of
            Element "td" attrs l -> Element "td" attrs [TextNode $ dispPkgVer v]

    tab = Element "table" [] rows

    hackurl = "https://hackage.haskell.org/package/" <> pkgname

    thPkgName = Element "th" [] [Element "a" [("href",hackurl)] [TextNode pkgname]]

    rows :: [Node]
    rows = [ Element "thead" [] [Element "tr" []
                                 (thPkgName
                                  : map thgv' gvs)]
           , Element "tbody" [] [ Element "tr" [] (row v) | v <- vs ]
           , Element "tfoot" [] [Element "tr" [] (Element "th" [("class","empty-lb")] [] : map thgv' gvs)]
           ]

    row v = thv v : [ lup v g | g <- gvs ]

    lup v g = case rdGVersions ^? ix g._2.ix v of
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

    thv :: PkgVer -> Node
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
        xrevt = "(" <> (tshow xrev) <> ")"

        xrevnode = [ Element "sup" [] [Element "a" [("class","xrev"),("href",revLogUrl),("title","revision log")]
                                       [TextNode xrevt]]
                   | xrev /= 0 ]

        n = pkgname
        vtxt = dispPkgVer v
        hdiffUrl = "http://hdiff.luite.com/cgit/" <> n <> "/commit?id=" <> vtxt
        hackUrl  = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/" <> n <> ".cabal/edit"
        revLogUrl = "https://hackage.haskell.org/package/" <> n <> "-" <> vtxt <> "/revisions"
