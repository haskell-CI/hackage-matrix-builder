#!/usr/bin/env runghc

-- Copyright (C) 2015  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           System.Directory (getAppUserDataDirectory)
import           System.Environment
import           System.IO (hSetBuffering, stdout, BufferMode(..))
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Char

import           Common

default (Text)

-- | Set of GHC executables
ghcExecutables :: [FilePath]
ghcExecutables = [ "/opt/ghc" <> v <> "bin/ghc" | v <- ["7.0.4","7.2.2","7.4.2","7.6.3","7.8.4","7.10.1"] ]

-- HACK
indexTar :: FilePath
indexTar = (fromText $ T.pack $ unsafePerformIO $ getAppUserDataDirectory "cabal") <> "packages/hackage.haskell.org/00-index.tar"
{-# INLINE indexTar #-}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [pkgn] <- liftIO getArgs
    shelly $ mainScript (T.pack pkgn)

mainScript :: Text -> Sh ()
mainScript pkgn = do
    pkgvs <- getPkgVersions pkgn
    echo $ "# " <> T.pack (show $ length pkgvs) <> " versions found: " <> T.intercalate " " (dispVer <$> pkgvs)
    forM_ pkgvs $ \pkgv -> do
        getCabalFile (pkgn,pkgv) >>= \case
            Nothing -> echo $ "STATUS:NO-RLS:" <> dispPkgId (pkgn,pkgv)
            Just t  -> case extractXRev t of
                0 -> return () -- uninteresting
                xrev -> echo $ "STATUS:XREV:" <> dispPkgId (pkgn,pkgv) <> "\t" <> (T.pack $ show xrev)
    forM_ (reverse ghcExecutables) $ doGhcVer pkgn pkgvs

doGhcVer :: PkgName -> [Version] -> FilePath -> Sh ()
doGhcVer pkgn pkgvs ghcbin = do
    [ghcver] <- liftM T.lines $ silently $ run ghcbin ["--numeric-version"]

    -- group package-versions by their extra install-deps;
    -- that way we can re-use the sandbox multiple times
    gips <- withTmpDir $ \tmpdir -> chdir tmpdir $ do
        resetSandbox ghcbin

        -- collect install-plans
        ips <- forM pkgvs $ \pkgv -> do
            let pkgid = dispPkgId (pkgn,pkgv)

            dryInstall ghcbin pkgid >>= \case
                Left _         -> return (pkgv, Left False) -- no install-plan
                Right []       -> return (pkgv, Left True)  -- nothing to do; already installed
                Right (_:deps) -> return (pkgv, Right deps)

        return [ (map fst xs, common)
               | xs@((_,common):_) <- groupBy ((==) `on` snd) $ sortBy (comparing snd) ips ]

    -- iterate through same-install-dep groups
    forM_ gips $ \case
        (vs, Left noop) -> forM_ vs $ \pkgv -> do
            -- 'Nothing' means there was no install-plan
            let pkgid = dispPkgId (pkgn,pkgv)
                echoStatus s deps cmt =
                    echo $ "STATUS:" <> s <> ":" <> pkgid <> "\tGHC-" <> ghcver <> "\t"
                                     <> T.intercalate " " deps
                                     <> (if cmt /= "" then " # " <> cmt else "")
            echoStatus (if noop then "PASS-NO-OP" else "PASS-NO-IP") [] ""

        (vs, Right deps) -> withTmpDir $ \tmpdir -> chdir tmpdir $ do
            resetSandbox ghcbin

            forM_ vs $ \pkgv -> do
                let pkgid = dispPkgId (pkgn,pkgv)
                    echoStatus s cmt =
                        echo $ "STATUS:" <> s <> ":" <> pkgid <> "\tGHC-" <> ghcver <> "\t"
                                         <> T.intercalate " " deps
                                         <> (if cmt /= "" then " # " <> cmt else "")

                echoStatus "INIT-DEP-BUILD" ""

                installDeps ghcbin pkgid >>= \case
                    Left e -> do
                        dumpOutput e
                        echoStatus "FAIL-DEP-BUILD" ""
                        -- TODO: assume FAIL-DEP-BUILD for other package-versions in same install-dep group

                    Right _ -> do
                        echoStatus "PASS-DEP-BUILD" ""
                        time (install ghcbin pkgid) >>= \case
                            (_, Left e) -> do
                                dumpOutput e
                                echoStatus "FAIL-BUILD" ""

                            (dt, Right _) -> do
                                let dt' = round (dt * 1000) :: Int
                                echoStatus "PASS-BUILD" (T.pack $ show dt' <> "ms")

  where
    dumpOutput (so,se) = do
        forM_ (T.lines so) $ \l -> echo ("STDOUT: " <> l)
        forM_ (T.lines se) $ \l -> echo ("STDERR: " <> l)

resetSandbox :: FilePath -> Sh ()
resetSandbox ghcbin = silently $ do
    _ghcbin' <- toTextWarn ghcbin
    cabal_ ["sandbox", "delete"]
    cabal_ ["sandbox", "init"]

getPkgVersions :: Text -> Sh [Version]
getPkgVersions pkgn = silently $ do
    out <- cabal [ "list"
                 , "--package-db=clear"
                 , "--package-db=global"
                 , "--simple-output", pkgn
                 ]

    return [ parseVer' v | l <- T.lines out, let [n,v] = T.split (==' ') l, n == pkgn ]

getCabalFile :: PkgId -> Sh (Maybe Text)
getCabalFile (pkgn,pkgv) = silently $ do
    tarfn <- toTextWarn indexTar
    let cabfn = mconcat [ pkgn, "/", dispVer pkgv, "/", pkgn, ".cabal" ]
    out <- errExit False $ silently $ run "tar" ["xOf", tarfn, cabfn]
    lastExitCode >>= \case
        0 -> return $ Just out
        2 -> return $ Nothing
        _ -> fail "getCabalFile: unexpected exit-code"

-- | Extract @x-revision@ property from cabal file
extractXRev :: Text -> Int
extractXRev s = case xRevLines of
    [l] -> let xrev = read $ T.unpack $ T.drop 11 l in
           if xrev > 0 then xrev else error "non-positive x-revision value"
    []  -> 0
    _   -> error "extractXRev: multiple x-revision lines"
  where
    xRevLines = filter (T.isPrefixOf "x-revision:") $ map (T.filter (not . isSpace) . T.toLower) $ T.lines s

installDeps :: FilePath -> Text -> Sh (Either (Text,Text) Text)
installDeps ghcbin pkgid = do
    ghcbin' <- toTextWarn ghcbin
    out <- errExit False $ silently $
           cabal [ "--require-sandbox", "install"
                 , "--max-backjumps=-1"
                 , "--with-ghc", ghcbin'
                 , "--dependencies-only"
                 , pkgid
                 ]
    outErr <- lastStderr

    lastExitCode >>= \case
        0 -> return $ Right out
        _ -> return $ Left (out,outErr)

install :: FilePath -> Text -> Sh (Either (Text,Text) Text)
install ghcbin pkgid = do
    ghcbin' <- toTextWarn ghcbin
    out <- errExit False $ silently $
           cabal [ "--require-sandbox", "install"
                 , "--max-backjumps=-1"
                 , "--with-ghc", ghcbin'
                 , pkgid
                 ]
    outErr <- lastStderr

    lastExitCode >>= \case
        0 -> return $ Right out
        _ -> return $ Left (out,outErr)

dryInstall :: FilePath -> Text -> Sh (Either (Text,Text) [Text])
dryInstall ghcbin pkgid = do
    ghcbin' <- toTextWarn ghcbin
    out <- errExit False $ silently $
           cabal [ "--require-sandbox", "install"
                 , "--with-ghc", ghcbin'
                 , "--max-backjumps=-1"
                 , "--dry", pkgid
                 ]
    outErr <- lastStderr

    lastExitCode >>= \case
        0 -> return $ Right (go out)
        _ -> return $ Left (out,outErr)
  where
    go out
      | T.isInfixOf "All the requested packages are already installed:" out = []
      | last out' /= pkgid = error "dryInstall: internal inconsistency"
      | otherwise          = reverse out'
      where
        out' = map (T.takeWhile (/= ' ')) $
               tail $
               dropWhile (not . T.isPrefixOf "In order, the following would be installed") $
               T.lines out

cabal :: [Text] -> Sh Text
cabal = run "cabal"

cabal_ :: [Text] -> Sh ()
cabal_ = run_ "cabal"
