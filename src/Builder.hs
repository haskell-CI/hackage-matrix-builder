{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Builder (defaultMain) where

import           BuildReport
import           BuildTypes

import           Development.Shake          hiding ((*>))
import           Development.Shake.Classes
-- import           Development.Shake.Command
import           Development.Shake.FilePath
-- import           Development.Shake.Util

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception          (evaluate)
import           Control.Lens               hiding ((<.>))
import           Control.Monad
import qualified Data.Aeson                 as J
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Char                  as C
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as Set
import           Data.String
import           Data.String.ToString
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as T
import           System.Directory           (createDirectory,
                                             getAppUserDataDirectory,
                                             getCurrentDirectory)
import qualified System.Directory           as D
import           System.IO
import           System.IO.Unsafe
import qualified Text.Parsec                as P
import qualified Text.Parsec.Text           as P
import           Text.Read                  (readMaybe)

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

newtype PkgAutoFlagsQ = PkgAutoFlagsQ PkgId
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
        let vs' = [ r | (n, v, r, _, _) <- vs, n == pkgn, v == pkgv ]
        case vs' of
            []  -> return Nothing
            [r] -> return (Just r)
            _   -> fail "getXRev: internal inconsistency"

    getAutoFlags <- addOracle $ \(PkgAutoFlagsQ (pkgn,pkgv)) -> do
        vs <- readXListFile ("report" </> toFP pkgn <.> "idx")
        let fls' = [ fls | (n, v, _, _, fls) <- vs, n == pkgn, v == pkgv ]
        case fls' of
            []  -> return Nothing
            [r] -> return (Just r)
            _   -> fail "getAutoFlags: internal inconsistency"

    let normPkgIdFlags (pkg,pkgfls) = do
            aflags <- maybe [] id <$> getAutoFlags (PkgAutoFlagsQ pkg)

            let aflags' = [ f | f <- aflags, pkgFlagName f `notElem` pkgfns ]
                afns   = map pkgFlagName aflags
                pkgfns = map pkgFlagName pkgfls

                pkgfls' = pkgfls ++ aflags'

            unless (all (`elem` afns) pkgfns) $
                fail (unwords $ [show pkg, ":", show pkgfls,"not-in", show aflags])

            -- liftIO $ print (pkg,pkgfls,aflags,aflags')

            return (pkg,pkgfls')

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

        let vs' = [ v | (n, v, _r, _, _) <- vs, n == pkgn ]

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
            , rdVersions  = Map.fromList [ (v,(r,u/=NormalPref)) | (n, v, r, u, _) <- vs, n == pkgn ]
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

        (sout,serr,iplan) <- dryInstall' gv pkgid []

        bwriteFileChanged (out -<.> "iplan_stdout") sout
        bwriteFileChanged (out -<.> "iplan_stderr") serr
        swriteFileChanged out iplan

    "report/*/*.solver" %> \out -> do
        let (pkgn,gv) = splitSolvPath out
            idxfn = "report" </> toFP pkgn <.> ".idx"

        vs4 <- readXListFile idxfn
        _ <- getHackageEtag HackageEtagQ

        let pkgvs = [ v | (n,v,_,_,_) <- vs4, n == pkgn ]

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

            InstallPlan pkgid' pkgflgs sbcfg _ -> do
                unless (pkgid' == pkgid) $
                    fail (unwords [show pkgid', "/=", show pkgid])

                let pkg0  = (pkgid,pkgflgs)
                    xdeps = computeXDeps pkg0 sbcfg

                forM_ xdeps $ \(p1,p1deps) -> do
                    let deps = [ (x,fromJust $ lookup x xdeps) | x <- p1deps ]
                    updateXDeps gv p1 deps
                    putLoudT [tshowPkgIdFlags p1, " ==> ", T.unwords (map tshowPkgIdFlags p1deps)]

                (sout, blog, rc) <- withTempDir $ \tmpdir -> runInstallXdeps tmpdir gv pkg0 xdeps normPkgIdFlags
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
            -- (pkgid,_) = unSbId sbid

        -- avoid rebuilding existing sandboxes
        outExists <- liftIO $ D.doesFileExist out
        unless outExists $ do
            (pkg0,xdeps) <- lookupXDeps gv sbid

            -- putNormal $ "...with deps: " ++ show (map (tshowPkgId . fst) xdeps)

            -- paranoia
            liftIO $ removeFiles sbdir ["//*"] >> createDirectory sbdir

            -- will create 'exit.code' file
            void $ runInstallXdeps sbdir gv pkg0 xdeps normPkgIdFlags

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
        command [ghcOpt, Traced "xcabal-install-dry"] xcabalExe $
        [ "--ignore-sandbox", "install"
        , "-v2"
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

    go sout serr = case soutlines of
        (_:"In order, the following would be installed:":depls) ->
            let deps = map (parseDepLine . decodeUtf8) depls
                goal = last deps
                subgs = init deps
            in InstallPlan (goal^._1) (goal^._2) subgs bpkgs

        (_:"All the requested packages are already installed:":depls) ->
            let deps = map (parsePkgId' . decodeUtf8 . BC.takeWhile (/= ' ')) depls
            in AlreadyInstalled (head deps)

        _ -> error ("dryInstall: unexpected output " ++ show soutlines)

      where
        soutlines = dropWhile (/= "Resolving dependencies...") $ BC.lines sout

        bpkgs = case dropWhile (not . B.isPrefixOf "Warning: The following packages are likely to be broken by the reinstalls:") $ BC.lines serr of
            (_:depls) -> map (parsePkgId' . decodeUtf8) depls
            []        -> []

        -- <pkgid> ('(latest: ' <lver> ')')?  ((+|-)<flag>)* ('(via: ' <pkgid>+ ')')? ...
        parseDepLine l = either (\e -> error (unlines ["parseDepLine", show l, show e])) id
                         . P.parse (depline <* P.eof) "" $ l
          where
            depline :: P.Parser  (PkgId, [PkgFlag], [PkgId], IPType) -- (T.Text, [T.Text], [T.Text])
            depline = do
                pid <- pkgid <* spc
                _ <- P.optionMaybe (P.try (P.string "(latest: ") *> pkgv <* P.string ") ")
                flgs <- (P.try flg) `P.endBy` spc
                -- _ <- stanza `P.endBy` spc
                dps <- P.option [] (P.try (P.string "(via: ") *> (pkgid `P.sepBy` spc) <* P.string ") ")
                ty <- P.choice [ P.try (P.string "(new package)") *> pure IPNewPkg
                               , P.try (P.string "(new version)") *> pure IPNewVer
                               ,        P.string "(reinstall)"    *> pure IPReInst
                                 <* P.optionMaybe (P.string " (changes: " *> P.many (P.noneOf ")") <* P.char ')')
                               ]

                pid' <-       maybe (fail "bad pkg-id") return . parsePkgId . T.pack $ pid
                dps' <- mapM (maybe (fail "bad pkg-id") return . parsePkgId . T.pack) dps

                return (pid', flgs, dps', ty)

            spc   = void (P.char ' ')
            pkgv  = P.many1 (P.satisfy (\c -> C.isDigit c || c == '.'))
            pkgid = P.many1 (P.satisfy (\c -> C.isAlphaNum c || c `elem` ['-','.']))
            flgid = P.many1 (P.satisfy (\c -> C.isAlphaNum c || c `elem` ['-','_']))
            flg = do
                b <- (=='+') <$> P.oneOf "+-"
                (if b then PkgFlagSet else PkgFlagUnset) . T.pack <$> flgid

----------------------------------------------------------------------------

-- | Install-plan represented as list of @(pkg, fwd-deps)@ pairs
--
-- the total list, as well as the fwd-deps lists are topologically sorted
type XDeps = [(PkgIdFlags,[PkgIdFlags])]

-- | Compute (transitive-flattened) forward-xdeps
computeXDeps :: PkgIdFlags -> [(PkgId, [PkgFlag], [PkgId], IPType)] -> XDeps
computeXDeps (pkgid0,_) ds =
    force [ (mkpidfl pid,map mkpidfl (getdeps pid)) | pid <- pkgids ]
  where
    pkgids = ds^..traversed._1

    topsort xs
      | Set.member pkgid0 xss = error "computeXDeps: cycle detected"
      | otherwise = [ p | p <- pkgids, Set.member p xss ]
      where
        xss = Set.fromList xs

    mkpidfl pid = Map.findWithDefault (error "computeXDeps") pid pkgflags
    pkgflags = Map.fromList [ (pid,(pid,pfls)) | (pid, pfls, _, _) <- ds ]

    -- list of pkg->pkg fwd-dep edges
    fdeps = [ (p1,p2) | (p2,_,p1s,_) <- ds, p1 <- p1s ]

    -- transitive closure; fdeps should be a DAG
    getdeps pid = topsort $ Map.findWithDefault [] pid tfdeps -- is top-sorted
    tfdeps = Map.fromListWith (++) . map (fmap (:[])) . Set.toAscList . transClosure . Set.fromList $ fdeps

transClosure :: Ord a => Set (a,a) -> Set (a,a)
transClosure clo0
  | clo0 == clo1  = clo0
  | otherwise     = transClosure clo1
  where
    clo1 = clo0 <> Set.fromList clo'
    -- new transitive edges
    clo' = [(a, c) | (a , b) <- Set.toList clo0
                   , (b', c) <- Set.toList clo0
                   , b == b'
                   ]


xdepsDict :: MVar (Map.Map (GhcVer,SbId) (PkgIdFlags,XDeps))
xdepsDict = unsafePerformIO $ newMVar (Map.empty)
{-# NOINLINE xdepsDict #-}

updateXDeps :: GhcVer -> PkgIdFlags -> XDeps -> Action ()
updateXDeps gv pkg xdeps = liftIO $ modifyMVar_ xdepsDict $ \d -> do
    case Map.lookup (gv,sbid) d of
        Just (pkg',xdeps') -> do -- NOOP
            unless (pkg' == pkg) $
                fail ("updateXDeps "++ show (gv,sbid) ++ ": " ++ show pkg' ++ " /= " ++ show pkg)
            unless (xdeps' == xdeps) $
                fail ("updateXDeps "++ show (gv,sbid) ++ ": " ++ show xdeps' ++ " /= " ++ show xdeps)
            return d
        Nothing -> evaluate (Map.insert (gv,sbid) (pkg,xdeps) d)
  where
    sbid = mkSbId pkg (map fst xdeps)

lookupXDeps :: GhcVer -> SbId -> Action (PkgIdFlags,XDeps)
lookupXDeps gv sbid = liftIO $ withMVar xdepsDict $ \d -> do
    case Map.lookup (gv,sbid) d of
        Nothing -> fail ("lookupXDeps "++ show (gv,sbid) ++ ": entry not found")
        Just (pkg,deps) -> do
            unless (mkSbId pkg (map fst deps) == sbid) $
                fail ("lookupXDeps "++ show (gv,sbid) ++ ": integrity check failed")
            return (pkg,deps)


-- | @cabal install@ with exactly the 'XDeps'-specified build-dependencies
runInstallXdeps :: FilePath -> GhcVer -> PkgIdFlags -> XDeps -> (PkgIdFlags -> Action PkgIdFlags)
                   -> Action (ByteString, ByteString, SbExitCode)
runInstallXdeps sbdir gv pkgid xdeps normPkgIdFlags = do
    putNormalT [ "creating ", T.pack (show gv), " sandbox for ", tshowPkgIdFlags pkgid, "  ("
               , T.unwords (map (tshowPkgIdFlags . fst) xdeps), ") at ", T.pack (show sbdir)
               ]

    -- maybe abort early and report only the first failed dep?
    need [ mkSbPath p1 p1deps </> "exit.code" | (p1,p1deps) <- xdeps ]

    oldcwd <- liftIO getCurrentDirectory

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

            -- should be enough to only normalise only the current package goal
            npkgid <- normPkgIdFlags pkgid

            let xdepcstrsv = concatMap (cstrsFromPkgIdFlags . fst) xdeps
                xdepcstrs  = xdepcstrsv <> [ (n,PkgCstrInstalled) | (n,_) <- xdepcstrsv ]

            -- do the build
            (Exit rc, Stdouterr sout) <- command [Cwd sbdir, ghcOpt, Traced "xcabal-install"] xcabalExe $
                [ "--require-sandbox", "install"
                , "--with-ghc", ghcbin
                , "--jobs=1"
                , "--reorder-goals"
                , "--max-backjumps=-1"
                , "--force-reinstalls"
                , "--report-planning-failure"
                , "--remote-build-reporting=detailed"
                , toString (pkgid^._1._1 :: PkgName)
                ]
                ++ concat [ [ "--constraint", showPkgCstr c ]
                          | c <- cstrsFromPkgIdFlags npkgid ++ xdepcstrs ]

            bwriteFileChanged (sbdir </> "build.stdouterr") sout

            blog <- liftIO $ B.readFile (sbdir </> ".cabal-sandbox" </> "logs" </> "build.log")
                -- TODO: parse build.log

            when (rc == ExitSuccess) $ do
                let tpkgid = tshowPkgId (fst pkgid)
                    installedPkgs = map (decodeUtf8 . BC.drop 10)
                                    . filter (BC.isPrefixOf "Installed ") . BC.lines $ sout

                -- simple sanity check
                case installedPkgs of
                    [] -> fail "runInstallXdeps: missing 'Installed'-line in output"
                    [tpkgid'] -> unless (tpkgid' == tpkgid) $
                                 fail (unwords [ "runInstallXdeps:"
                                               , show tpkgid', "/=", show tpkgid
                                               ])
                    (_:_:_) -> fail ("runInstallXdeps: installedPkgs=" ++ show installedPkgs)

                (Exit rc', Stdout pkgdump) <- command [ Cwd sbdir, ghcOpt, EchoStderr False
                                                      , Traced "cabal-sandbox-describe"]
                                              cabalExe [ "sandbox", "hc-pkg", "--", "describe"
                                                       , T.unpack tpkgid ]
                case rc' of
                    ExitSuccess -> bwriteFileChanged (sbdir </> "pkg.cfg") (fixupPkgDump pkgdump)
                    ExitFailure _ ->
                        -- TODO: analyse 'sout' to verify non-library package
                        putNormalT ["WARNING: no package ", tpkgid, " registered; assuming non-library package"]

            -- TODO: validate resulting sandbox matches sandbox-specification

            let src = case rc of
                    ExitSuccess   -> SbExitOk
                    ExitFailure _ -> SbExitFail []

            swriteFileChanged (sbdir </> "exit.code") src

            return $!! (sout,blog,src)
  where
    mkSbPath p1 p1deps = "sandboxes" </> toFP (mkSbId p1 p1deps) </> toFP gv <.> "d"

    ghcbin = ghcBinPath gv <> "ghc"

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
readXListFile :: FilePath -> Action [(PkgName, PkgVer, PkgRev, PkgVerStatus, [PkgFlag])]
readXListFile fn = do
    lns <- treadFileLines fn
    return $!! map (go . T.words) lns
  where
    go (n0:v0:r0:d0:fls) = (PkgName n0, parsePkgVer' v0, tread r0, readPkgPref d0, map readPkgFlag fls)
    go _             = error "readXListFile"

    readPkgPref "N" = NormalPref
    readPkgPref "U" = UnPreferred
    readPkgPref t   = error ("readPkgPref "++show t)

    readPkgFlag t = case T.uncons t of
        Just ('-', n) -> PkgFlagUnset n
        Just ('+', n) -> PkgFlagSet   n
        _             -> error ("readPkgFlag " ++ show t)

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
