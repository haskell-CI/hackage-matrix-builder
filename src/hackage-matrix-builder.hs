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
import           System.Environment

default (Text)

type PkgName = Text
type PkgVer  = Text

-- | Set of GHC executables
ghcExecutables :: [FilePath]
ghcExecutables = [ "/opt/ghc" <> v <> "bin/ghc" | v <- ["7.0.4","7.2.2","7.4.2","7.6.3","7.8.4","7.10.1"] ]

main :: IO ()
main = do
    [pkgn] <- liftIO getArgs
    shelly $ mainScript (T.pack pkgn)

mainScript :: Text -> Sh ()
mainScript pkgn = do
    pkgvs <- getPkgVersions pkgn
    echo $ "# " <> T.pack (show $ length pkgvs) <> " versions found: " <> T.intercalate " " pkgvs
    forM_ (reverse ghcExecutables) $ doGhcVer pkgn pkgvs

doGhcVer :: PkgName -> [PkgVer] -> FilePath -> Sh ()
doGhcVer pkgn pkgvs ghcbin = do
    [ghcver] <- liftM T.lines $ silently $ run ghcbin ["--numeric-version"]

    -- group package-versions by their extra install-deps;
    -- that way we can re-use the sandbox multiple times
    gips <- withTmpDir $ \tmpdir -> chdir tmpdir $ do
        resetSandbox ghcbin

        -- collect install-plans
        ips <- forM pkgvs $ \pkgv -> do
            let pkgid = pkgn <> "-" <> pkgv

            dryInstall ghcbin pkgid >>= \case
                Left _         -> return (pkgv, Nothing) -- no install-plan
                Right []       -> return (pkgv, Nothing) -- already installed (FIXME)
                Right (_:deps) -> return (pkgv, Just deps)

        return [ (map fst xs, common)
               | xs@((_,common):_) <- groupBy ((==) `on` snd) $ sortBy (comparing snd) ips ]

    -- iterate through same-install-dep groups
    forM_ gips $ \case
        (vs, Nothing) -> forM_ vs $ \pkgv -> do
            -- 'Nothing' means there was no install-plan
            let pkgid = pkgn <> "-" <> pkgv
                echoStatus s deps cmt =
                    echo $ "STATUS:" <> s <> ":" <> pkgid <> "\tGHC-" <> ghcver <> "\t"
                                     <> T.intercalate " " deps
                                     <> (if cmt /= "" then " # " <> cmt else "")
            echoStatus "PASS-NO-IP" [] ""

        (vs, Just deps) -> withTmpDir $ \tmpdir -> chdir tmpdir $ do
            resetSandbox ghcbin

            forM_ vs $ \pkgv -> do
                let pkgid = pkgn <> "-" <> pkgv
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

getPkgVersions :: Text -> Sh [Text]
getPkgVersions pkgn = silently $ do
    out <- cabal ["list", "--simple-output", pkgn]
    return [ v | l <- T.lines out, let [n,v] = T.split (==' ') l, n == pkgn ]


installDeps :: FilePath -> Text -> Sh (Either (Text,Text) Text)
installDeps ghcbin pkgid = do
    ghcbin' <- toTextWarn ghcbin
    out <- errExit False $ silently $
           cabal [ "--require-sandbox", "install"
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
