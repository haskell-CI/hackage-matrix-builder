{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Builder (defaultMain) where

import           BuildReport
import           BuildTypes

import           Development.Shake
import           Development.Shake.Classes
-- import           Development.Shake.Command
import           Development.Shake.FilePath
-- import           Development.Shake.Util

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.String.ToString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (decodeUtf8)
import           System.Directory (getAppUserDataDirectory, createDirectory, createDirectoryIfMissing, getCurrentDirectory)
import qualified System.Directory as D
import           System.IO
import           System.IO.Unsafe
import           Text.Read (readMaybe)

----------------------------------------------------------------------------
-- Filesystem configuration

-- | Path to @cabal-install@'s @00-index.tar@ package index
indexTar :: FilePath
indexTar = unsafePerformIO (getAppUserDataDirectory "cabal") </> "packages/hackage.haskell.org/00-index.tar"
{-# NOINLINE indexTar #-}

-- | Path to @cabal@ frontend (tested only with 1.22)
cabalExe :: FilePath
cabalExe = "/opt/cabal/1.22/bin/cabal"

-- | @xcabal@ frontend (get it from https://github.com/hvr/xcabal)
xcabalExe :: FilePath
xcabalExe = "xcabal"

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

newtype PkgRevQ = PkgRevQ PkgId
              deriving (Eq,Show,NFData,Generic,Hashable,Binary)

data HackageEtagQ = HackageEtagQ
                  deriving (Eq,Show,NFData,Generic,Hashable,Binary)

newtype GhcVerQ = GhcVerQ GhcVer
                deriving (Eq,Show,NFData,Generic,Hashable,Binary)

----------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = shakeArgs shakeOptions {- shakeFiles="_build/" -} $ do
    -- want ["00-index.lst"]

    -- getPkgList <- addOracle $ \ghcv -> do
    --     Stdout out <- command [] (ghcBinPath ghcv <> "ghc-pkg") ["list", "--simple-output", "--global"]
    --     return [(reverse b, reverse a) | x <- words out, let (a,_:b) = break (== '-') $ reverse x]

    getGhcVer <- addOracle $ \(GhcVerQ gv) -> do
        Stdout out <- command [] (ghcBinPath gv <> "ghc") ["--numeric-version"]
        maybe (fail "getGhcVer") return $ parsePkgVer (T.dropWhileEnd (`elem` [' ','\n','\r']) (decodeUtf8 out))

    getXRev <- addOracle $ \(PkgRevQ (pkgn,pkgv)) -> do
        vs <- readXListFile ("report" </> toFP pkgn <.> "idx")
        let vs' = [ r | (n, v, r, _) <- vs, n == pkgn, v == pkgv ]
        case vs' of
            []  -> return Nothing
            [r] -> return (Just r)
            _   -> fail "getXRev: internal inconsistency"

    getHackageEtag <- addOracle $ \HackageEtagQ ->
        sreadFile (indexTar <.> "gz" <.> "etag")

    -- not needed currently
    "00-index.lst" %> \out -> do
        need [indexTar]
        putNormal "generating 00-index.lst..."
        Stdout lstdata <- command [] xcabalExe ["xlist"]
        bwriteFileChanged out lstdata

    "report/*.idx" %> \out -> do
        let pkgn = takeBaseName out
        when (null pkgn) $ fail "bad package name"

        need [indexTar]
        Stdout lstdata <- command [] xcabalExe ["xlist",pkgn]
        when (B.null lstdata) $ fail ("unknown package " ++ show pkgn)
        bwriteFileChanged out lstdata

    "report/*.html" %> \out -> do
        rd <- jreadFile (out -<.> ".json")
        cssData <- decodeUtf8 <$> breadFile "style.css"
        bwriteFileChanged out $ docToBS $ genHtmlReport (Right cssData) rd

    "report/*.json" %> \out -> do
        let idxfn = out -<.> ".idx"
            pkgn  = fromString (takeBaseName out)

        vs <- readXListFile idxfn

        let vs' = [ v | (n, v, _r, _) <- vs, n == pkgn ]

        unless (length vs == length vs') $
            fail "internal error"

        need $ [ "report" </> toFP pkgn </> toFP v </> toFP gv <.> "result"
               | v <- vs',  gv <- ghcVers  ] ++
               [ "report" </> toFP pkgn </> toFP gv <.> "solver" | gv <- ghcVers ]

        matrix <- forM ghcVers $ \gv -> do
            gfullver <- getGhcVer (GhcVerQ gv)

            sdata <- sreadFile ("report" </> toFP pkgn </> toFP gv <.> "solver") :: Action SolverData

            let sdata' = map (fmap solveResultToPkgVer) $
                         ([], fst sdata) : [ ([v1,v2], sr) | ((v1,v2), sr) <- snd sdata ]

            pvdata <- forM vs' $ \pv ->
                (,) pv <$> sreadFile ("report" </> toFP pkgn </> toFP pv </>
                                      toFP gv <.> "result") :: Action (PkgVer,BuildResult)

            return (gv, (gfullver, Map.fromList pvdata, Map.fromList sdata'))

        -- TODO
        jwriteFileChanged out ReportData
            { rdPkgName   = pkgn
            , rdVersions  = Map.fromList [ (v,(r,u)) | (n, v, r, u) <- vs, n == pkgn ]
            , rdGVersions = Map.fromList matrix
            }

    -- actually just a dependency install-plan
    "report/*/*/*.iplan" %> \out -> do
        let (pkgn,pkgv,gv) = splitRepPath out
            pkgid = (pkgn,pkgv)

        etag <- decodeUtf8 <$> getHackageEtag HackageEtagQ

        Just xrev <- getXRev (PkgRevQ pkgid)

        putLoudT [ "(Re)generating iplan for ", tshowPkgId pkgid, "~", T.pack (show xrev)
                 , " (Hackage-Etag = ", etag, ")"
                 ]

        (sout,serr,tmp) <- dryInstall' gv pkgid []

        bwriteFileChanged (out -<.> "iplan_stdout") sout
        bwriteFileChanged (out -<.> "iplan_stderr") serr
        swriteFileChanged out tmp

    "report/*/*.solver" %> \out -> do
        let (pkgn,gv) = splitSolvPath out
            idxfn = "report" </> toFP pkgn <.> ".idx"

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
                    putLoudT [tshowPkgId p1, " ==> ", T.unwords (map tshowPkgId p1deps)]
                    updateXDeps p1 [ (x,fromJust $ lookup x xdeps) | x <- p1deps ]

                (sout, blog, rc) <- withTempDir $ \tmpdir -> runInstallXdeps tmpdir gv pkgid xdeps
                bwriteFileChanged (out -<.> "build_stdout") sout
                bwriteFileChanged (out -<.> "build_log") blog

                case rc of
                    SbExitOk         -> return BuildOk
                    SbExitFail []    -> return $ BuildFail (decodeUtf8 sout)
                    SbExitFail fsbids@(_:_) -> do
                        rcs <- forM fsbids $ \fsbid -> do
                            let sbdir = "sandboxes" </> toFP fsbid </> toFP gv <.> "d"
                            sout' <- breadFile (sbdir </> "build.stdouterr")
                            return (fst (unSbId fsbid), decodeUtf8 sout')
                        return $ BuildFailDeps rcs

        -- need ["00-index.lst"]
        swriteFileChanged out res

    "sandboxes/*/*.d/exit.code" %> \out -> do
    -- "sandboxes/*/*.d/cabal.sandbox.config"
    -- "sandboxes/*/*.d/.cabal-sandbox/logs/build.log"
        let (sbid,gv) = splitSbPath out
            sbdir = "sandboxes" </> toFP sbid </> toFP gv <.> "d"
            (pkgid,_) = unSbId sbid

        -- avoid rebuilding existing sandboxes
        outExists <- liftIO $ D.doesFileExist out
        unless outExists $ do
            xdeps <- lookupXDeps sbid

            -- putNormal $ "...with deps: " ++ show (map (tshowPkgId . fst) xdeps)

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
        [ "--ignore-sandbox", "install"
        , "--global"
        , "--with-ghc", ghcbin
        , "--max-backjumps=-1"
        , "--force-reinstalls"
        , "--dry"
        , "--reorder-goals"
        , toString pkgname
        ] ++ concat [ ["--constraint", showPkgCstr nc] | nc <- constraints ]

    let !res = case rc of
            ExitSuccess -> go sout serr
            ExitFailure n -> NoInstallPlan (fromIntegral n) (sout<>serr)

    return $!! (sout,serr,res)
  where
    ghcbin = ghcBinPath gv <> "ghc"

    go sout serr
        | B.isInfixOf "All the requested packages are already installed:" sout = AlreadyInstalled (head out2)
--      | last out' /= pkgid = error $ "dryInstall: internal inconsistency" ++ show (out', pkgid)
        | otherwise          = InstallPlan (last out') (init out') bpkgs
      where
        out' = map (parsePkgId' . decodeUtf8 . BC.takeWhile (/= ' ')) $
               tail $
               dropWhile (not . B.isPrefixOf "In order, the following would be installed") $
               BC.lines sout

        out2 = map (parsePkgId' . decodeUtf8 . BC.takeWhile (/= ' ')) $
               tail $
               dropWhile (not . B.isPrefixOf "All the requested packages are already installed:") $
               BC.lines sout


        bpkgs | B.isInfixOf "Warning: The following packages are likely to be broken by the reinstalls:" serr
              = map (parsePkgId' . decodeUtf8) $
                tail $
                dropWhile (not . B.isPrefixOf "Warning: The following packages are likely to be broken by the reinstalls:") $
                BC.lines serr
              | otherwise = []

----------------------------------------------------------------------------

type XDeps = [(PkgId, [PkgId])]

updateXDeps :: PkgId -> XDeps -> Action ()
updateXDeps pkgid xdeps
    = unless (null xdeps) $ do
        liftIO $ createDirectoryIfMissing False "deps"
        swriteFileChanged ("deps" </> toFP (mkSbId pkgid deps) <.> "xdeplist") xdeps
  where
    deps = map fst xdeps

lookupXDeps :: SbId -> Action XDeps
lookupXDeps sbid
  | hashDeps [] == snd (unSbId sbid) = pure []
  | otherwise = do
      let (_,dephash) = unSbId sbid
      xdeps <- sreadFile ("deps" </> toFP sbid <.> "xdeplist")
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
    putNormalT [ "creating ", T.pack (show gv), " sandbox for ", tshowPkgId pkgid, "  ("
               , T.unwords (map (tshowPkgId . fst) xdeps), ") at ", T.pack (show sbdir)
               ]

    -- maybe abort early and report only the first failed dep?
    need [ mkSbPath p1 p1deps </> "exit.code" | (p1,p1deps) <- xdeps ]

    oldcwd <- liftIO getCurrentDirectory

    -- find out if additional constraints are needed
    xtraCstrs <- computeAllXtraCstrs

    -- initialise sandbox
    ghcOpt <- addGhcPath gv
    command_ [Cwd sbdir, ghcOpt, EchoStdout False, Traced "cabal-sandbox-init"] cabalExe [ "sandbox", "init" ]

    -- populate sandbox w/ deps
    exitCodes <- forM xdeps $ \(p1,p1deps) ->
        (,) (mkSbId p1 p1deps) <$> sreadFile (mkSbPath p1 p1deps </> "exit.code")

    let failedSbs = nub . sort . concat $
                    [ if null fsbids then [fsbid1] else fsbids
                    | (fsbid1, SbExitFail fsbids) <- exitCodes
                    ]

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
                , "--reorder-goals"
                , "--max-backjumps=-1"
                , "--force-reinstalls"
                , "--report-planning-failure"
                , "--remote-build-reporting=detailed"
                , T.unpack (tshowPkgId pkgid)
                ]
                ++ concat [ [ "--constraint", showPkgCstr c ] | c <- xdepcstrs++xtraCstrs ]

            bwriteFileChanged (sbdir </> "build.stdouterr") sout

            blog <- liftIO $ B.readFile (sbdir </> ".cabal-sandbox" </> "logs" </> "build.log")
                -- TODO: parse build.log

            when (rc == ExitSuccess) $ do
                (Exit rc', Stdout pkgdump) <- command [ Cwd sbdir, ghcOpt, EchoStderr False
                                                      , Traced "cabal-sandbox-describe"]
                                              cabalExe [ "sandbox", "hc-pkg", "--", "describe"
                                                       , T.unpack (tshowPkgId pkgid) ]
                case rc' of
                    ExitSuccess -> bwriteFileChanged (sbdir </> "pkg.cfg") (fixupPkgDump pkgdump)
                    ExitFailure _ ->
                        -- TODO: analyse 'sout' to verify non-library package
                        putNormalT ["WARNING: no package ", tshowPkgId pkgid, " registered; assuming non-library package"]

            -- TODO: validate resulting sandbox matches sandbox-specification

            let src = case rc of
                    ExitSuccess   -> SbExitOk
                    ExitFailure _ -> SbExitFail []

            swriteFileChanged (sbdir </> "exit.code") src

            return $!! (sout,blog,src)
  where
    mkSbPath p1 p1deps = "sandboxes" </> toFP (mkSbId p1 p1deps) </> toFP gv <.> "d"

    xdeppkgns = map (fst . fst) xdeps
    xdepcstrsv = [ (n,PkgCstrEq v) | ((n,v),_) <- xdeps ]
    xdepcstrs  = xdepcstrsv <> [ (n,PkgCstrInstalled) | (n,_) <- xdepcstrsv ]

    ghcbin = ghcBinPath gv <> "ghc"

    -- | 'xdepcstrsv' may leave some DOF, here we compute extra cstrs
    -- needed to keep additional packages from sneaking into the
    -- build-plan
    computeAllXtraCstrs = go []
      where
        go xcstrs0 = do
            xcstrs <- computeXtraCstrsStep xcstrs0
            case xcstrs of
                []    -> return xcstrs0
                (_:_) -> do
                    putNormal $ "WARNING: extra dependencies sneaked in: " ++ show (map fst xcstrs)
                    go (xcstrs0++xcstrs)

    computeXtraCstrsStep xcstrs0 = do
        -- find out if additional constraints are needed
        (_,_,InstallPlan _pkg0 x _) <- dryInstall' gv pkgid (xdepcstrsv++xcstrs0)
        let x' = map fst x
        unless (null $ xdeppkgns \\ x') $
            fail "runInstallXdeps: integrity check failed"
        return [ (n,PkgCstrInstalled) | n <- x' \\ xdeppkgns ]


----------------------------------------------------------------------------
-- misc helpers

-- bshow :: Show a => a -> ByteString
-- bshow = BC.pack . show

-- bread :: Read a => ByteString -> a
-- bread bs = fromMaybe (error $ "bread " <> show bs) . readMaybe . BC.unpack $ bs

tread :: Read a => Text -> a
tread t = fromMaybe (error $ "tread " <> show t) . readMaybe . T.unpack $ t

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

{-
sreadFileIO :: Read a => FilePath -> IO a
sreadFileIO fn = do
    raw <- readFile fn
    case readMaybe raw of
        Just v -> return v
        Nothing -> fail $ "failed to sreadFileIO " ++ show fn
-}

-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
breadFile :: FilePath -> Action B.ByteString
breadFile x = need [x] >> liftIO (B.readFile x)

-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
treadFile :: FilePath -> Action Text
treadFile x = need [x] >> liftIO (T.readFile x)

-- | A version of 'treadFile'' which also splits the result into
-- lines.  The argument file will be tracked as a dependency.
treadFileLines :: FilePath -> Action [Text]
treadFileLines = fmap T.lines . treadFile

-- | Read JSON data
jreadFile :: FromJSON a => FilePath -> Action a
jreadFile fn = do
    raw <- breadFile fn
    case J.decodeStrict' raw of
        Just v -> return v
        Nothing -> fail $ "failed to jreadFile " ++ show fn

-- | Write JSON data
jwriteFileChanged :: ToJSON a => FilePath -> a -> Action ()
jwriteFileChanged fn = bwriteFileChanged fn . BL.toStrict . J.encode



-- | ...
readXListFile :: FilePath -> Action [(PkgName, PkgVer, PkgRev, Bool)]
readXListFile fn = do
    lns <- treadFileLines fn
    return $!! map (go . T.words) lns
  where
    go [n0,v0,r0,d0] = (PkgName n0, parsePkgVer' v0, tread r0, tread d0)
    go _             = error "readXListFile"

-- | ...
splitRepPath :: FilePath -> (PkgName, PkgVer, GhcVer)
splitRepPath fp = force (pkgn,pkgv,gv)
  where
    fp'  = dropDirectory1 fp
    pkgn = fromString $ takeDirectory1 fp'
    fp'' = dropDirectory1 fp'
    pkgv = parsePkgVer' . fromString . takeDirectory1 $ fp''
    gv   = parseGhcVer' . fromString . takeBaseName $ fp''

splitSolvPath :: FilePath -> (PkgName, GhcVer)
splitSolvPath fp = force (pkgn,gv)
  where
    fp'  = dropDirectory1 fp
    (pkgn,fp'') = (fromString $ takeDirectory1 fp', dropDirectory1 fp')
    gv   = parseGhcVer' . fromString . dropExtension $ fp''

-- | ...
splitSbPath :: FilePath -> (SbId, GhcVer)
splitSbPath fp = force (sbid,gv)
  where
    fp'  = dropDirectory1 fp
    sbid = SbId $ T.pack $ takeDirectory1 fp'
    gvs  = dropExtension $ takeDirectory1 $ dropDirectory1 fp'
    gv   = parseGhcVer' (T.pack gvs)


fixupPkgDump :: ByteString -> ByteString
fixupPkgDump = (`BC.append` "\n") . fst . BC.breakSubstring "\n---"

putLoudT, putNormalT :: [Text] -> Action ()
putLoudT   = putLoud   . T.unpack . mconcat
putNormalT = putNormal . T.unpack . mconcat
