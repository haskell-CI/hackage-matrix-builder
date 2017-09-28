{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module Controller.Compute ( runCompute ) where

import           Prelude.Local

import qualified Database.PostgreSQL.Simple       as PGS
import           Database.PostgreSQL.Simple.Types (Only (..))
-- import qualified Database.PostgreSQL.Simple.Types as PGS

-- local modules
import           PkgId
import qualified PkgIdxRanges as IdxRanges
import           PkgIdxRanges (IdxRanges)


runCompute :: PGS.Connection -> IO ()
runCompute dbconn = do
    -- xs <- PGS.query_ dbconn "SELECT unnest(units) FROM solution JOIN iplan_job USING (jobid) \
    --                         \EXCEPT \
    --                         \SELECT xunitid FROM solution_span WHERE not vstale \
    --                         \"

    -- xs <- PGS.query_ dbconn "SELECT xunitid FROM iplan_unit where pkind <> 'builtin' \
    --                         \EXCEPT \
    --                         \SELECT xunitid FROM solution_span WHERE not vstale \
    --                         \"

    _ <- PGS.execute_ dbconn "SET work_mem = '64MB'"

    -- TODO/FIXME: empty pkgindex
    [(ptime_watermark, ptime_max)] <- PGS.query_ dbconn "SELECT (SELECT ptime_watermark FROM globals),max(ptime) FROM pkgindex"

    print (ptime_watermark :: PkgIdxTs, ptime_max :: PkgIdxTs)

    newstales <- PGS.query dbconn
             "WITH t1 as (SELECT pname,pver,prev FROM pkgindex WHERE ptime > ? AND ptime <= ? AND prev > 0), \
                  \t2 as (SELECT DISTINCT xunitid FROM t1 JOIN iplan_unit using (pname,pver) \
                                                         \JOIN solution_rev sr USING (xunitid) \
                                                         \WHERE sr.prev = t1.prev-1) \
                  \UPDATE solution_span ss SET vstale='t' FROM t2 WHERE ss.xunitid = t2.xunitid \
                  \RETURNING ss.xunitid"
             (ptime_watermark, ptime_max)

    when False $
        print (newstales :: [Only UUID])

    -- forM_ newstales $ \(Only xuid) -> do
    --     putStrLn ("-- " ++ show xuid ++ " -----------------------------------------------------------------")
    --     _ <- querySpanUpd xuid
    --     pure ()


    putStrLn "============================================================================================================="

    whileM_ $ do
        xs <- PGS.query_ dbconn "SELECT xunitid FROM solution_span WHERE vstale"
        forM_ xs $ \(Only xuid) -> do
            putStrLn ("-- " ++ show xuid ++ " -----------------------------------------------------------------")
            _ <- querySpanUpd xuid
            pure ()
        pure (not (null xs))

    _ <- PGS.execute dbconn "UPDATE globals SET ptime_watermark = ?" (Only ptime_max)

    putStrLn "== ptime watermark updated"

    when True $ do
        [Only new_sol_rev_cnt] <- PGS.query_ dbconn "SELECT sync_solution_rev(1000)"

        print (new_sol_rev_cnt :: Int)

        whileM_ $ do
            putStrLn "============================================================================================================="

            xs <- PGS.query_ dbconn "SELECT xunitid FROM solution_rev \
                                    \EXCEPT \
                                    \SELECT xunitid FROM solution_span WHERE not vstale \
                                    \LIMIT 1000"

            putStrLn (show (length xs) ++ " solutions fetched...")

            forM_ xs $ \(Only xuid) -> do
                putStrLn ("-- " ++ show xuid ++ " -----------------------------------------------------------------")
                _res <- querySpanUpd xuid
                -- print res
                pure ()

            putStrLn (show (length xs) ++ " solutions fetched...")

            pure (not (null xs)) -- 'True' if this iteration did some work
  where
    {-
    spanExists :: UUID -> IO Bool
    spanExists xuid = do
        [Only b] <- PGS.query dbconn "SELECT EXISTS(SELECT FROM solution_span WHERE xunitid = ? AND not vstale)" (Only xuid)
        pure b

    -- | return non-stale spans (if they exist)
    querySpan :: UUID -> IO (Maybe IdxRanges)
    querySpan xuid = do
        s <- PGS.query dbconn "SELECT valid FROM solution_span WHERE xunitid = ? AND not vstale" (Only xuid)
        case s of
          (Only r):_ -> pure (Just r)
          _          -> pure Nothing
    -}

    -- | like 'querySpan' but recompute if stale
    --
    -- Invariant: UUID must exist
    --
    -- May recurse; marks parents stale if needed
    querySpanUpd :: UUID -> IO IdxRanges
    querySpanUpd xid = do
        (pname,pver,_) <- fetchUnit xid
        snd <$> querySpanUpd' xid pname pver

    querySpanUpd' :: UUID -> PkgN -> Ver -> IO (PkgIdxTs, IdxRanges)
    querySpanUpd' xid pname pver  = do
        res <- PGS.query dbconn "SELECT vstale,valid,ptime0 FROM solution_span WHERE xunitid = ?" (Only xid)
        case res of
          (False,ir,t0):_    -> pure (t0,ir) -- done; there's a n entry, and it's already current
          []                 -> doUpd Nothing -- missing entry; create one
          (True,old_ir,t0):_ -> doUpd (Just (t0,old_ir)) -- stale entry; update
      where
        doUpd mold_t0_ir = do
            -- retrieve child ranges
            cs <- fetchUnitDeps xid
            (ct0s,cirs) <- unzip <$> (forM cs $ \(cxid,cpname,cpver) -> querySpanUpd' cxid cpname cpver)

            -- compute local ranges
            (local_t0,local_ir) <- lookupRanges xid pname pver
            let new_ir = IdxRanges.intersectionN (local_ir :| cirs)
                new_t0 = maximum (local_t0 : ct0s)

            case mold_t0_ir of
              Nothing -> pure ()
              Just (_,old_ir) | old_ir == new_ir -> putStrLn ("unchanged: " ++ show new_ir)
                              | otherwise        -> putStrLn ("  CHANGED: " ++ show old_ir ++ " --> " ++ show new_ir)

            case mold_t0_ir of
              Just (old_t0,_) | old_t0 /= new_t0 -> fail (show (old_t0,new_t0))
              _ -> pure ()

            _ <- PGS.execute dbconn "INSERT INTO solution_span (xunitid,valid,vstale,ptime0) VALUES (?,?,?,?) \
                                    \ ON CONFLICT (xunitid) \
                                    \ DO UPDATE SET valid = EXCLUDED.valid, vstale = EXCLUDED.vstale \
                                    \" (xid,new_ir,False,new_t0)

            unless (fmap snd mold_t0_ir == Just new_ir) $ do
                -- range changed; mark parents stale
                case mold_t0_ir of
                  Just _ -> do
                      n <- PGS.execute dbconn "UPDATE solution_span SET vstale = 't' FROM iplan_comp_dep WHERE child = ? AND xunitid = parent AND not vstale"
                          (Only xid)

                      when (n > 0) $ putStrLn ("** Marked " ++ show n ++ " parents STALE! **")

                  Nothing -> do
                      [Only ex] <- PGS.query dbconn "SELECT EXISTS(SELECT FROM solution_span ss, iplan_comp_dep d WHERE child = ? AND ss.xunitid = parent)" (Only xid)

                      when ex $ fail ("internal invariant broken (parents existed already for new entry!) " ++ show xid)

                -- -- TODO: *If* we trust invariants: if this is a new entry, there won't be any parents in solution_span yet
                -- when (isNothing mold_ir && n /= 0) $ -- note: 'n' doesn't include already stale parents!
                --     fail "internal invariant broken (parents existed already for new entry!)"

                pure ()

            pure (new_t0,new_ir)

    fetchUnit :: UUID -> IO (PkgN,Ver,Bool)
    fetchUnit xuid = do
        [x] <- PGS.query dbconn "SELECT pname,pver,pkind='builtin' FROM iplan_unit WHERE xunitid = ?" (Only xuid)
        pure x

    -- non-builtins
    fetchUnitDeps :: UUID -> IO [(UUID,PkgN,Ver)]
    fetchUnitDeps xuid = PGS.query dbconn "SELECT DISTINCT xunitid,pname,pver FROM iplan_unit JOIN iplan_comp_dep ON (child = xunitid) WHERE parent = ? AND pkind <> 'builtin'" (Only xuid)

    ---------------------------------------------------------------------------------------
    -- lookup ranges for a xid based on 'solution_rev' + 'pkgindex'

    lookupRanges :: UUID -> PkgN -> Ver -> IO (PkgIdxTs, IdxRanges)
    lookupRanges xid pname pver = do
        revs <- PGS.query dbconn "SELECT prev FROM solution_rev WHERE xunitid = ? ORDER BY prev" (Only xid) :: IO [(Only Int)]
        (Just t0, rgs) <- lookupRevs2 pname pver (map fromOnly revs)
        pure (t0,rgs)

    lookupRevs2 :: PkgN -> Ver -> [Int] -> IO (Maybe PkgIdxTs, IdxRanges)
    lookupRevs2 pname pver revs0 = do
        rs <- lookupRevs1 pname pver
        let t0 = case rs of
                   [] -> Nothing
                   (t',_):_ -> Just t'

        pure $ (t0, IdxRanges.fromList (go' 0 (sort revs0) rs))
      where
        go' _ []    _  = []
        go' _ (_:_) [] = error "lookupRevs2"
        go' j (j1:js) (x:xs) = case compare j j1 of
                                 EQ -> x : go' (j+1)     js  xs
                                 LT ->     go' (j+1) (j1:js) xs
                                 GT -> error "lookupRevs2'"

    lookupRevs1 :: PkgN -> Ver -> IO [(PkgIdxTs,Maybe PkgIdxTs)]
    lookupRevs1 pname pver = do
        res0 <- PGS.query dbconn "SELECT prev,ptime FROM pkgindex WHERE pname = ? AND pver = ? ORDER BY prev" (pname,pver)
        let ptimes = map snd (res0 :: [(Int,PkgIdxTs)])
        -- TODO: assert consecutive rev nums
        pure (zipWith (,) ptimes (map Just (tail ptimes) ++ [Nothing]))


