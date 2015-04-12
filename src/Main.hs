{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           BuildReport
import           BuildTypes

import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           System.Directory (getAppUserDataDirectory, createDirectory, createDirectoryIfMissing, getCurrentDirectory)
import qualified System.Directory as D
import           System.IO
import           System.IO.Unsafe
import           Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

----------------------------------------------------------------------------

indexTar :: FilePath
indexTar = unsafePerformIO (getAppUserDataDirectory "cabal") </> "packages/hackage.haskell.org/00-index.tar"
{-# NOINLINE indexTar #-}

cabalExe :: FilePath
cabalExe = "/opt/cabal/1.22/bin/cabal"

-- | Path where to find @ghc@ and @ghc-pkg@.
ghcBinPath :: GhcVer -> FilePath
ghcBinPath v = "/opt/ghc/" ++ v' ++ "/bin/"
  where
    v' = case v of
        GHC_7_00 -> "7.0.4"
        GHC_7_02 -> "7.2.2"
        GHC_7_04 -> "7.4.2"
        GHC_7_06 -> "7.6.3"
        GHC_7_08 -> "7.8.4"
        GHC_7_10 -> "7.10.1"

addGhcPath :: GhcVer -> Action CmdOption
addGhcPath gv = addPath [ghcBinPath gv] []


----------------------------------------------------------------------------
-- oracle wrappers

newtype XRevQ = XRevQ PkgId
              deriving (Eq,Show,NFData,Generic,Hashable,Binary)

newtype XRevA = XRevA (Maybe PkgRev)
              deriving (Eq,Show,NFData,Generic,Hashable,Binary)

data HackageEtagQ = HackageEtagQ
                  deriving (Eq,Show,NFData,Generic,Hashable,Binary)

newtype GhcVerQ = GhcVerQ GhcVer
                deriving (Eq,Show,NFData,Generic,Hashable,Binary)

----------------------------------------------------------------------------

main :: IO ()
main = shakeArgs shakeOptions {- shakeFiles="_build/" -} $ do
    -- want ["00-index.lst"]

    -- getPkgList <- addOracle $ \ghcv -> do
    --     Stdout out <- command [] (ghcBinPath ghcv <> "ghc-pkg") ["list", "--simple-output", "--global"]
    --     return [(reverse b, reverse a) | x <- words out, let (a,_:b) = break (== '-') $ reverse x]

    getGhcVer <- addOracle $ \(GhcVerQ gv) -> do
        Stdout out <- command [] (ghcBinPath gv <> "ghc") ["--numeric-version"]
        maybe (fail "getGhcVer") return $ parsePkgVer (dropWhileEnd (`elem` [' ','\n','\r']) out)

    getXRev <- addOracle $ \(XRevQ (PkgName pkgn,pkgv)) -> do
        vs <- readXListFile ("report" </> pkgn <.> "idx")
        let vs' = [ r | (PkgName n, v, r, _) <- vs, n == pkgn, v == pkgv ]
        case vs' of
            []  -> return $ XRevA Nothing
            [r] -> return $ XRevA (Just r)
            _   -> fail "getXRev: internal inconsistency"

    getHackageEtag <- addOracle $ \HackageEtagQ ->
        sreadFile (indexTar <.> "gz" <.> "etag")

    -- not needed currently
    "00-index.lst" %> \out -> do
        need [indexTar]
        putNormal "generating 00-index.lst..."
        Stdout lstdata <- command [] "xcabal" ["xlist"]
        bwriteFileChanged out lstdata

    "report/*.idx" %> \out -> do
        let pkgn = takeBaseName out
        when (null pkgn) $ fail "bad package name"

        need [indexTar]
        Stdout lstdata <- command [] "xcabal" ["xlist",pkgn]
        when (B.null lstdata) $ fail ("unknown package " ++ show pkgn)
        bwriteFileChanged out lstdata

    "report/*.html" %> \out -> do
        let datafn = out -<.> ".data"
        rd <- sreadFile datafn

        bwriteFileChanged out $ docToBS $ genHtmlReport rd

    "report/*.data" %> \out -> do
        let idxfn = out -<.> ".idx"
            pkgn  = PkgName (takeBaseName out)

        vs <- readXListFile idxfn

        let vs' = [ v | (n, v, _r, _) <- vs, n == pkgn ]
            vstrs = map showPkgVer vs'

        unless (length vs == length vs') $
            fail "internal error"

        need $ [ "report" </> unPkgName pkgn </> pv </> ghcVerStr gv <.> "result" | pv <- vstrs, gv <- ghcVers  ] ++
               [ "report" </> unPkgName pkgn </> ghcVerStr gv <.> "solver" | gv <- ghcVers ]

        matrix <- forM ghcVers $ \gv -> do
            gfullver <- getGhcVer (GhcVerQ gv)

            sdata <- sreadFile ("report" </> unPkgName pkgn </> ghcVerStr gv <.> "solver") :: Action SolverData

            let sdata' = map (fmap solveResultToPkgVer) $
                         ([], fst sdata) : [ ([v1,v2], sr) | ((v1,v2), sr) <- snd sdata ]

            pvdata <- forM vs' $ \pv ->
                (,) pv <$> sreadFile ("report" </> unPkgName pkgn </> showPkgVer pv </> ghcVerStr gv <.> "result") :: Action (PkgVer,BuildResult)

            let pvdata' = map (fmap brToStat) pvdata

            return (gv, (gfullver, Map.fromList pvdata', Map.fromList sdata'))

        -- TODO
        swriteFileChanged out $ ReportData
            { rdPkgName  = pkgn
            , rdVersions = Map.fromList [ (v,(r,u)) | (n, v, r, u) <- vs, n == pkgn ]
            , rdGVersions = Map.fromList matrix
            }

    -- actually just a dependency install-plan
    "report/*/*/*.iplan" %> \out -> do
        let (pkgn,pkgv,gv) = splitRepPath out
            pkgid = (pkgn,pkgv)

        etag <- BC.unpack <$> getHackageEtag HackageEtagQ

        XRevA (Just xrev) <- getXRev (XRevQ pkgid)

        putLoud $ "(Re)generating iplan for " <> showPkgId pkgid <> "~" <> show xrev
                  <> " (Hackage-Etag = " <> etag <> ")"

        (sout,serr,tmp) <- dryInstall' gv pkgid []

        bwriteFileChanged (out -<.> "iplan_stdout") sout
        bwriteFileChanged (out -<.> "iplan_stderr") serr
        swriteFileChanged out tmp

    "report/*/*.solver" %> \out -> do
        let (pkgn,gv) = splitSolvPath out
            idxfn = "report" </> unPkgName pkgn <.> ".idx"

        vs4 <- readXListFile idxfn
        _ <- getHackageEtag HackageEtagQ

        let pkgvs = [ v | (n,v,_,_) <- vs4, n == pkgn ]

        majSols <- forM (sort $ nub $ map majorVer pkgvs) $ \(v1,v2) -> do
            (_,_,rc) <- dryInstall gv pkgn [(pkgn, PkgCstrPfx [v1,v2])]
            return ((v1,v2), dryToSolveResult rc)

        (_,_,rc) <- dryInstall gv pkgn []
        let anySol = dryToSolveResult rc

        swriteFileChanged out ((anySol, majSols) :: SolverData)

    "report/*/*/*.result" %> \out -> do
        let (pkgn,pkgv,gv) = splitRepPath out
            pkgid = (pkgn,pkgv)

        iplan <- sreadFile (out -<.> ".iplan")

        res <- case iplan of
            AlreadyInstalled _ -> return BuildNop
            NoInstallPlan _ _  -> return BuildNoIp

            InstallPlan _ sbcfg _ -> do
                xdeps <- computeXDeps gv sbcfg

                forM_ xdeps $ \(p1,p1deps) -> do
                    putLoud $ showPkgId p1 <> " ==> " <> unwords (map showPkgId p1deps)
                    updateXDeps p1 [ (x,fromJust $ lookup x xdeps) | x <- p1deps ]

                (sout, blog, rc) <- withTempDir $ \tmpdir -> runInstallXdeps tmpdir gv pkgid xdeps
                bwriteFileChanged (out -<.> "build_stdout") sout
                bwriteFileChanged (out -<.> "build_log") blog

                return $ case rc of
                    SbExitOk         -> BuildOk
                    SbExitFail []    -> BuildFail
                    SbExitFail (_:_) -> BuildFailDeps

        -- need ["00-index.lst"]
        swriteFileChanged out res

    "sandboxes/*/*.d/exit.code" %> \out -> do
    -- "sandboxes/*/*.d/cabal.sandbox.config"
    -- "sandboxes/*/*.d/.cabal-sandbox/logs/build.log"
        let (sbid,gv) = splitSbPath out
            sbdir = "sandboxes" </> sbid </> ghcVerStr gv <.> "d"
            (pkgid,_) = unSbId sbid

        -- avoid rebuilding existing sandboxes
        outExists <- liftIO $ D.doesFileExist out
        unless outExists $ do
            xdeps <- lookupXDeps sbid

            -- putNormal $ "...with deps: " ++ show (map (showPkgId . fst) xdeps)

            -- paranoia
            liftIO $ removeFiles sbdir ["//*"] >> createDirectory sbdir

            -- will create 'exit.code' file
            void $ runInstallXdeps sbdir gv pkgid xdeps

    -- phony "clean" $ do
    --     putNormal "Cleaning files..."
    --     removeFilesAfter "sandboxes" ["//*"]

----------------------------------------------------------------------------

dryInstall' :: GhcVer -> PkgId -> [(PkgName,PkgCstr)] -> Action (ByteString,ByteString,DryResult)
dryInstall' gv pkgid constraints = dryInstall gv (fst pkgid) (cstrFromPkgId pkgid:constraints)

-- | compute install-plan with global package database only
dryInstall :: GhcVer -> PkgName -> [(PkgName,PkgCstr)] -> Action (ByteString,ByteString,DryResult)
dryInstall gv pkgname constraints = do -- withTempDir $ \tmpdir -> do
    ghcOpt <- addGhcPath gv
    -- command_ [Cwd tmpdir, ghcOpt, EchoStdout False, Traced "cabal-sandbox-init"] cabalExe [ "sandbox", "init" ]
    (Exit rc, Stdout sout, Stderr serr) <-
        command [ghcOpt, Traced "cabal-install-dry"] cabalExe $
        [ "install" -- "--require-sandbox"
        , "--global"
        , "--with-ghc", ghcbin
        , "--max-backjumps=-1"
        , "--force-reinstalls"
        , "--dry"
        , "--reorder-goals"
        , unPkgName pkgname
        ] ++ concat [ ["--constraint", showPkgCstr nc] | nc <- constraints ]

    let !res = case rc of
            ExitSuccess -> go sout serr
            ExitFailure n -> NoInstallPlan (fromIntegral n) (sout<>serr)

    return $!! (sout,serr,res)
  where
    ghcbin = ghcBinPath gv <> "ghc"

    go sout serr
        | B.isInfixOf "All the requested packages are already installed:" sout = AlreadyInstalled (head out2)
     -- | last out' /= pkgid = error $ "dryInstall: internal inconsistency" ++ show (out', pkgid)
        | otherwise          = InstallPlan (last out') (init out') bpkgs
      where
        out' = map (parsePkgId' . BC.unpack) $
               map (BC.takeWhile (/= ' ')) $
               tail $
               dropWhile (not . B.isPrefixOf "In order, the following would be installed") $
               BC.lines sout

        out2 = map (parsePkgId' . BC.unpack) $
               map (BC.takeWhile (/= ' ')) $
               tail $
               dropWhile (not . B.isPrefixOf "All the requested packages are already installed:") $
               BC.lines sout


        bpkgs | B.isInfixOf "Warning: The following packages are likely to be broken by the reinstalls:" serr
              = map (parsePkgId' . BC.unpack) $
                tail $
                dropWhile (not . B.isPrefixOf "Warning: The following packages are likely to be broken by the reinstalls:") $
                BC.lines serr
              | otherwise = []

----------------------------------------------------------------------------

type XDeps = [(PkgId, [PkgId])]

updateXDeps :: PkgId -> XDeps -> Action ()
updateXDeps pkgid xdeps
    = unless (null xdeps) $
      swriteFileChanged ("deps" </> mkSbId pkgid deps <.> "xdeplist") xdeps
  where
    deps = map fst xdeps

lookupXDeps :: SbId -> Action XDeps
lookupXDeps sbid
  | hashDeps [] == snd (unSbId sbid) = pure []
  | otherwise = do
      let (_,dephash) = unSbId sbid
      xdeps <- sreadFile ("deps" </> sbid <.> "xdeplist")
      unless (hashDeps (map fst xdeps) == dephash) $
          fail ("lookupXDeps "++ show sbid ++ ": integrity check failed")
      return xdeps

-- TODO: extend 'cabal install --dry' to print out dep-graph to avoid calling multiple times into cabal
computeXDeps :: GhcVer -> [PkgId] -> Action XDeps
computeXDeps gv sbcfg = do
    let cstrs = map cstrFromPkgId sbcfg
    forM sbcfg $ \p1 -> do
        (_,_,InstallPlan p1' p1deps _) <- dryInstall' gv p1 cstrs
        unless (p1 == p1') $ fail "WTF"
        return (p1, p1deps)

runInstallXdeps :: FilePath -> GhcVer -> PkgId -> XDeps -> Action (ByteString, ByteString, SbExitCode)
runInstallXdeps sbdir gv pkgid xdeps = do
    putNormal $ concat [ "creating ", show gv, " sandbox for ", showPkgId pkgid, "  ("
                       , unwords (map (showPkgId . fst) xdeps), ") at ", show sbdir
                       ]

    ghcOpt <- addGhcPath gv
    oldcwd <- liftIO getCurrentDirectory

    command_ [Cwd sbdir, ghcOpt, EchoStdout False, Traced "cabal-sandbox-init"] cabalExe [ "sandbox", "init" ]

    -- populate sandbox w/ deps

    let mkSbPath p1 p1deps = "sandboxes" </> mkSbId p1 p1deps </> ghcVerStr gv <.> "d"

    -- maybe abort early and report only the first failed dep?
    need [ mkSbPath p1 p1deps </> "exit.code" | (p1,p1deps) <- xdeps ]

    exitCodes <- forM xdeps $ \(p1,p1deps) ->
        (,) (mkSbId p1 p1deps) <$> sreadFile (mkSbPath p1 p1deps </> "exit.code")

    let failedSbs = [ sbid1 | (sbid1, SbExitFail _) <- exitCodes ]

    case failedSbs of
        sbs@(_:_) -> do
            let src = SbExitFail sbs
            swriteFileChanged (sbdir </> "exit.code") src
            return $!! ("","",src)

        [] -> do

            forM_ xdeps $ \(p1,p1deps) -> do
                let pkgcfg = mkSbPath p1 p1deps </> "pkg.cfg"
                command_ [Cwd sbdir, ghcOpt, EchoStdout False, EchoStderr False, Traced "cabal-sandbox-register"]
                    cabalExe [ "sandbox", "hc-pkg", "--", "register", oldcwd </> pkgcfg ]

            -- do the build
            (Exit rc, Stdouterr sout) <- command [Cwd sbdir, ghcOpt, Traced "cabal-install"] cabalExe $
                [ "--require-sandbox", "install"
                , "--with-ghc", ghcbin
                , "--jobs=1"
                , "--max-backjumps=-1"
                , "--force-reinstalls"
                , "--report-planning-failure"
                , "--remote-build-reporting=detailed"
                , showPkgId pkgid
                ]
                ++ concat [ [ "--constraint", showPkgCstr (n,PkgCstrInstalled)
                            , "--constraint", showPkgCstr (n,PkgCstrEq v)
                            ]
                          | ((n,v),_) <- xdeps ]

            bwriteFileChanged (sbdir </> "build.stdouterr") sout

            blog <- liftIO $ B.readFile (sbdir </> ".cabal-sandbox" </> "logs" </> "build.log")
                -- TODO: parse build.log

            when (rc == ExitSuccess) $ do
                (Stdout pkgdump) <- command [Cwd sbdir, ghcOpt, Traced "cabal-sandbox-describe"]
                                    cabalExe [ "sandbox", "hc-pkg", "--", "describe", showPkgId pkgid ]
                bwriteFileChanged (sbdir </> "pkg.cfg") (fixupPkgDump pkgdump)

            -- TODO: validate resulting sandbox matches sandbox-specification

            let src = case rc of
                    ExitSuccess -> SbExitOk
                    ExitFailure _ -> SbExitFail []

            swriteFileChanged (sbdir </> "exit.code") src

            return $!! (sout,blog,src)
  where
    ghcbin = ghcBinPath gv <> "ghc"

----------------------------------------------------------------------------
-- misc helpers

bshow :: Show a => a -> ByteString
bshow = BC.pack . show

bread :: Read a => ByteString -> a
bread bs = fromMaybe (error $ "bread " <> show bs) . readMaybe . BC.unpack $ bs

-- | Write a file, but only if the contents would change.
bwriteFileChanged :: FilePath -> B.ByteString -> Action ()
bwriteFileChanged name !x = liftIO $ do
    b <- D.doesFileExist name
    if not b then B.writeFile name x else do
        b' <- withFile name ReadMode $ \h -> do
            src <- B.hGetContents h
            evaluate $ src /= x
        when b' $ B.writeFile name x

swriteFileChanged :: Show a => FilePath -> a -> Action ()
swriteFileChanged fn = writeFileChanged fn . (++"\n") . show

sreadFile :: Read a => FilePath -> Action a
sreadFile fn = do
    raw <- readFile' fn
    case readMaybe raw of
        Just v -> return v
        Nothing -> fail $ "failed to sreadFile " ++ show fn

sreadFileIO :: Read a => FilePath -> IO a
sreadFileIO fn = do
    raw <- readFile fn
    case readMaybe raw of
        Just v -> return v
        Nothing -> fail $ "failed to sreadFileIO " ++ show fn

-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
breadFile :: FilePath -> Action B.ByteString
breadFile x = need [x] >> liftIO (B.readFile x)

-- | ...
readXListFile :: FilePath -> Action [(PkgName, PkgVer, PkgRev, Bool)]
readXListFile fn = do
    lns <- readFileLines fn
    return $!! map (go . words) lns
  where
    go [n0,v0,r0,d0] = (PkgName n0, parsePkgVer' v0, read r0, read d0)
    go _             = error "readXListFile"

-- | ...
splitRepPath :: FilePath -> (PkgName, PkgVer, GhcVer)
splitRepPath fp = force (pkgn,pkgv,gv)
  where
    fp'  = dropDirectory1 fp
    pkgn = PkgName $ takeDirectory1 fp'
    fp'' = dropDirectory1 fp'
    pkgv = parsePkgVer' $ takeDirectory1 fp''
    gv   = parseGhcVer' $ takeBaseName fp''

splitSolvPath :: FilePath -> (PkgName, GhcVer)
splitSolvPath fp = force (pkgn,gv)
  where
    fp'  = dropDirectory1 fp
    (pkgn,fp'') = (PkgName $ takeDirectory1 fp', dropDirectory1 fp')
    gv   = parseGhcVer' $ dropExtension fp''

-- | ...
splitSbPath :: FilePath -> (SbId, GhcVer)
splitSbPath fp = force (sbid,gv)
  where
    fp'  = dropDirectory1 fp
    sbid = takeDirectory1 fp'
    gvs  = dropExtension $ takeDirectory1 $ dropDirectory1 fp'
    gv   = parseGhcVer' gvs


fixupPkgDump :: ByteString -> ByteString
fixupPkgDump = (`BC.append` "\n") . fst . BC.breakSubstring "\n---"
