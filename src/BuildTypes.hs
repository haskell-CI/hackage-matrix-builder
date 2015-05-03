{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Build-types and associated primitive helpers
module BuildTypes
    ( module BuildTypes
    , ByteString
    , ExitCode(..)
    , Generic
    , NFData
    , Natural
    , Binary
    , Set.Set
    , Map.Map
    , Text
    , FromJSON
    , ToJSON
    ) where

import           Control.DeepSeq
import           Control.Monad
import qualified Crypto.Hash.SHA256           as SHA256
import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Aeson                   as J
import           Data.Bifunctor
import           Data.Binary
import           Data.Bitraversable
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base16       as B16
import qualified Data.ByteString.Char8        as BC
import           Data.Coerce
import           Data.Hashable
-- import           Data.List
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as Set
import           Data.String
import           Data.String.ToString
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Binary             ()
import qualified Data.Text.Encoding           as T
import           Data.Version
import           GHC.Generics
import           Numeric.Natural
import           System.Exit
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S)

-- orphans
deriving instance Generic ExitCode
deriving instance NFData ExitCode

-- | Class for types which have a 'FilePath'(-component) representation
class ToFP s where
    toFP :: s -> FilePath

----------------------------------------------------------------------------

-- | Known GHC versions
data GhcVer = GHC_7_00
            | GHC_7_02
            | GHC_7_04
            | GHC_7_06
            | GHC_7_08
            | GHC_7_10
            deriving (Eq,Ord,Bounded,Enum,Read,Show,Hashable,Binary,NFData,Generic,FromJSON,ToJSON)

ghcVers :: [GhcVer]
ghcVers = [minBound..maxBound]

instance ToFP GhcVer where
    toFP = drop 4 . show

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer s = Map.lookup s tab
  where
    tab = Map.fromList [ (ghcVerStr v, v) | v <- [minBound..maxBound] ]
    ghcVerStr = T.pack . drop 4 . show

parseGhcVer' :: Text -> GhcVer
parseGhcVer' s = fromMaybe (error $ "parseGhcVer " ++ show s) . parseGhcVer $ s

----------------------------------------------------------------------------

newtype PkgName = PkgName Text
                deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Binary,FromJSON,ToJSON)

instance IsString PkgName where
    fromString = PkgName . fromString

instance ToString PkgName where
    toString (PkgName t) = toString t

instance ToFP PkgName where
    toFP = T.unpack . coerce

-- | Our variant of 'Data.Version.Version'
newtype PkgVer = PkgVer [Word]
               deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Binary,FromJSON,ToJSON)

instance ToFP PkgVer where
    toFP = T.unpack . tshowPkgVer

-- TODO: optimise 'T.pack . show' part
tshowPkgVer :: PkgVer -> Text
tshowPkgVer (PkgVer vs) = T.intercalate "." (map (T.pack . show) vs)

-- | Dual to 'tshowPkgVer'
parsePkgVer :: Text -> Maybe PkgVer
parsePkgVer v0 = do
    Version v [] <- runReadP parseVersion (T.unpack v0)
    PkgVer <$> mapM toIntegralSized v
  where
    runReadP :: ReadP a -> String -> Maybe a
    runReadP p s = listToMaybe [ x | (x,"") <- readP_to_S p s ]

-- | Non-total 'parsePkgVer'
parsePkgVer' :: Text -> PkgVer
parsePkgVer' s = fromMaybe (error $ "parsePkgVer " ++ show s) . parsePkgVer $ s

majMinVer :: PkgVer -> (Word,Word,Word)
majMinVer (PkgVer v) = case v of
    []        -> (0,0,0)
    [a]       -> (a,0,0)
    [a,b]     -> (a,b,0)
    (a:b:c:_) -> (a,b,c)

majorVer :: PkgVer -> (Word,Word)
majorVer v = let (a,b,_) = majMinVer v in (a,b)

----------------------------------------------------------------------------

type PkgRev  = Word

type PkgId   = (PkgName,PkgVer)

data PkgFlag = PkgFlagSet !Text
             | PkgFlagUnset !Text
             deriving (Show,Read,Eq,Ord,Generic,NFData,FromJSON,ToJSON,Hashable,Binary)

pkgFlagName :: PkgFlag -> Text
pkgFlagName (PkgFlagSet   t) = t
pkgFlagName (PkgFlagUnset t) = t

type PkgIdFlags = (PkgId,[PkgFlag])

tshowPkgFlag :: PkgFlag -> Text
tshowPkgFlag (PkgFlagSet t)    = T.cons '+' t
tshowPkgFlag (PkgFlagUnset t)  = T.cons '-' t

tshowPkgIdFlags :: PkgIdFlags -> Text
tshowPkgIdFlags (pkgid,flgs) = tshowPkgId pkgid <> case flgs of
    []    -> ""
    (_:_) -> T.cons ':' (T.intercalate "," (map tshowPkgFlag flgs))


tshowPkgId :: PkgId -> Text
tshowPkgId (PkgName n,v)
  | T.null vs = n -- version-less package id
  | otherwise = mconcat [n, "-", vs]
  where
    vs = tshowPkgVer v

parsePkgId :: Text -> Maybe PkgId
parsePkgId s = do
    guard (not $ T.null pkgn_)
    pkgv <- parsePkgVer pkgvs
    let pkgid = (PkgName (T.init pkgn_),pkgv)
    guard (tshowPkgId pkgid == s)
    return pkgid
  where
    (pkgn_,pkgvs) = T.breakOnEnd "-" s

parsePkgId' :: Text -> PkgId
parsePkgId' s = fromMaybe (error $ "parsePkgId " ++ show s) . parsePkgId $ s

----------------------------------------------------------------------------

newtype SbId = SbId Text -- <pkgname>-<pkgver>-<dephash>
             deriving (Eq,Ord,Generic,NFData)

instance Show SbId where
    showsPrec p (SbId t) = showsPrec p t

instance Read SbId where
    readsPrec p = coerce (readsPrec p :: ReadS Text)

instance ToFP SbId where
    toFP = T.unpack . coerce

mkSbId :: PkgIdFlags -> [PkgIdFlags] -> SbId
mkSbId dep0@(pkgid,_) deps = SbId $ mconcat [tshowPkgId pkgid, "_", coerce (hashDeps $ dep0:deps)]

unSbId :: SbId -> (PkgId,DepHash)
unSbId = force . bimap parsePkgId' (DepHash . T.tail) . T.break (=='_') . coerce

----------------------------------------------------------------------------

data IPType = IPNewPkg | IPNewVer | IPReInst
            deriving (Show,Read,Eq,Generic,NFData)

data DryResult
    = AlreadyInstalled !PkgId -- ^ target already installed, nothing to do
    | InstallPlan      !PkgId [PkgFlag] [(PkgId,[PkgFlag],[PkgId],IPType)] [PkgId]
      -- ^ install-plan for target, required sub-targets, and
      -- reinstall-affected packages
    | NoInstallPlan (Maybe Word) !ByteString -- ^ failed to find valid install plan
    | NoInstallPlanError !Int !ByteString !ByteString -- ^ abnormal output
    deriving (Show,Read,Generic,NFData)

type SolverData = (SolveResult, [((Word,Word),SolveResult)])

data SolveResult
    = SolveNoOp !PkgVer
    | SolveInstall !PkgVer
    | SolveNoInstall
    | SolveNoInstallBjLim !Word
    | SolveError
    deriving (Show,Read,Generic,NFData,FromJSON,ToJSON)

solveResultToPkgVer :: SolveResult -> Maybe PkgVer
solveResultToPkgVer = \case
    SolveNoOp v           -> Just v
    SolveInstall v        -> Just v
    SolveNoInstall        -> Nothing
    SolveNoInstallBjLim _ -> Nothing
    SolveError            -> Nothing

dryToSolveResult :: DryResult -> SolveResult
dryToSolveResult = \case
    AlreadyInstalled pid       -> SolveNoOp (snd pid)
    InstallPlan pid _ _ _      -> SolveInstall (snd pid)
    NoInstallPlan Nothing _    -> SolveNoInstall
    NoInstallPlan (Just bjs) _ -> SolveNoInstallBjLim bjs
    NoInstallPlanError _ _ _   -> SolveError

data SbExitCode
    = SbExitOk
    | SbExitFail [SbId] -- list contains indirect failures, i.e. the
                        -- ids of direct and indirect build-deps whose
                        -- indirect-failure list was empty
    deriving (Show,Read,Generic,NFData)

-- | Represents build outcome status with associated meta-info
data BuildResult
    = BuildOk
    | BuildNop
    | BuildNoIp
    | BuildNoIpBjLimit !Word -- ^ no install-plan found due to max-backjumps limit
    | BuildNoIpFail !Text !Text -- ^ cabal failed w/ abnormal status
    | BuildFail !Text -- build-output
    | BuildFailDeps [(PkgId,Text)] -- failed deps & associated build-outputs
    deriving (Show,Read,Generic,NFData,FromJSON,ToJSON)

-- | Hash of package-dependencies constructed via 'hashDeps'
newtype DepHash = DepHash Text
                deriving (Eq,Ord,Generic,NFData)

hashDeps :: [PkgIdFlags] -> DepHash
hashDeps = DepHash . T.decodeUtf8 . B16.encode . SHA256.hash . BC.pack . show

type PkgVerPfx = [Word]

-- | Represents preferred-versions information
data PkgVerStatus = NormalPref | UnPreferred | Deprecated
                  deriving (Eq,Show,Read,Generic,NFData,FromJSON,ToJSON)

data PkgCstr = PkgCstrEq  !PkgVer
             | PkgCstrPfx PkgVerPfx  -- ^ @PkgCstrPfx []@ means no constraint at all
             | PkgCstrInstalled
             | PkgCstrFlag !PkgFlag
             deriving (Show,Read,Generic,NFData,Eq,Ord,FromJSON,ToJSON)

cstrFromPkgId :: PkgId -> (PkgName,PkgCstr)
cstrFromPkgId = bimap id PkgCstrEq

cstrsFromPkgIdFlags :: PkgIdFlags -> [(PkgName,PkgCstr)]
cstrsFromPkgIdFlags (pid,fls) = cstrFromPkgId pid : [(fst pid,PkgCstrFlag fl)|fl<-fls]

tshowPkgCstr :: (PkgName,PkgCstr) -> Text
tshowPkgCstr (PkgName n,cstr) = case cstr of
    PkgCstrEq v      -> n <> " ==" <> tshowPkgVer v
    PkgCstrPfx v     -> n <> " ==" <> T.intercalate "." (map showWord v ++ ["*"])
    PkgCstrInstalled -> n <> " installed"
    PkgCstrFlag f    -> n <> " " <> tshowPkgFlag f
  where
    showWord :: Word -> Text
    showWord = T.pack . show

-- | Legacy version of 'tshowPkgCstr'
showPkgCstr :: (PkgName,PkgCstr) -> String
showPkgCstr = T.unpack . tshowPkgCstr

----------------------------------------------------------------------------
-- Some semi-orphans (they can only become full orphans, if aeson
-- decides to define instances paremtric in the key-type of the Map,
-- which it doesn't right now) to support encoding 'ReportData'

instance ToJSON v => ToJSON (Map.Map PkgVer v) where
    toJSON = J.toJSON . Map.toList

instance FromJSON v => FromJSON (Map.Map PkgVer v) where
    parseJSON v = Map.fromList <$> J.parseJSON v

----

-- | Since GhcVer gets encoded to a string anyway, we can use JSON
-- objects for GhcVer-indexed maps
instance ToJSON v => ToJSON (Map.Map GhcVer v) where
    toJSON = J.toJSON . Map.fromList . map f . Map.toList
      where
        f (J.toJSON -> J.String l,v) = (l,v)
        f _                          = error "ToJSON(Map GhcVer _)"

instance FromJSON v => FromJSON (Map.Map GhcVer v) where
    parseJSON v = do
        entries <- Map.toList <$> J.parseJSON v
        Map.fromList <$> mapM (bitraverse (J.parseJSON . J.String) pure) entries

----

instance ToJSON v => ToJSON (Map.Map PkgVerPfx v) where
    toJSON = J.toJSON . Map.toList

instance FromJSON v => FromJSON (Map.Map PkgVerPfx v) where
    parseJSON v = Map.fromList <$> J.parseJSON v
