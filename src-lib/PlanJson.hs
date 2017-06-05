{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module PlanJson
    ( PlanJson(..)
    , PIType(..)
    , PlanItem(..)
    , CompName(..), parseCompName, dispCompName, strCompName
    , CompInfo(..)
    ) where

import           Prelude.Local

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import           Data.Text                          (Text)
import qualified Data.Text                          as T
-- import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import           PkgId

-- TODO: "bin-file", "dist-dir"

data PlanJson = PlanJson
     { pjCabalVersion    :: Ver
     , pjCabalLibVersion :: Ver
     , pjCompilerId      :: PkgId
     , pjArch            :: Text -- TODO
     , pjOs              :: Text -- TODO
     , pjItems           :: M.Map UnitID PlanItem
     } deriving Show

data PIType = PIBuiltin
            | PIGlobal
            | PILocal
            | PILocal2
            deriving (Show,Eq,Ord)

data PlanItem = PlanItem
     { piId     :: UnitID
     , piPId    :: PkgId
     , piType   :: PIType
     , piFlags  :: M.Map Text Bool
     , piSha256 :: Maybe Text -- TODO
     , piComps  :: M.Map CompName CompInfo
     } deriving Show

-- | Component of a package
--
-- NB: a similiar type exists in cabal's codebase
data CompName
    = CompNameLib
    | CompNameSubLib Text
    | CompNameExe    Text
    | CompNameTest   Text
    | CompNameBench  Text
    | CompNameSetup
    deriving (Show, Eq, Ord)

data CompInfo = CompInfo
    { ciLibDeps :: S.Set UnitID
    , ciExeDeps :: S.Set UnitID
    } deriving Show

----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

instance ToField CompName where
    toField = toField . dispCompName

instance FromJSON CompName where
    parseJSON = withText "CompName" $
        (maybe (fail "invalid CompName") pure . parseCompName)

instance ToJSON CompName where
    toJSON = toJSON . dispCompName
    toEncoding = toEncoding . dispCompName

instance FromJSONKey CompName where
    fromJSONKey = FromJSONKeyTextParser
                  (maybe (fail "CompName") pure . parseCompName)

instance ToJSONKey   CompName where
    toJSONKey = toJSONKeyText dispCompName

instance FromJSON CompInfo where
    parseJSON = withObject "CompInfo" $ \o ->
        CompInfo <$> o .:?! "depends"
                 <*> o .:?! "exe-depends"




parseCompName :: Text -> Maybe CompName
parseCompName t0 = case T.splitOn ":" t0 of
                     ["lib"]     -> Just $ CompNameLib
                     ["lib",n]   -> Just $ CompNameSubLib n
                     ["exe",n]   -> Just $ CompNameExe n
                     ["bench",n] -> Just $ CompNameBench n
                     ["test",n]  -> Just $ CompNameTest n
                     ["setup"]   -> Just $ CompNameSetup
                     _           -> Nothing


strCompName :: CompName -> Maybe Text
strCompName = \case
    CompNameLib       -> Nothing
    CompNameSubLib n  -> Just n
    CompNameExe n     -> Just n
    CompNameBench n   -> Just n
    CompNameTest n    -> Just n
    CompNameSetup     -> Nothing

dispCompName :: CompName -> Text
dispCompName = \case
    CompNameLib       -> "lib"
    CompNameSubLib n  -> "lib:" <> n
    CompNameExe n     -> "exe:" <> n
    CompNameBench n   -> "bench:" <> n
    CompNameTest n    -> "test:" <> n
    CompNameSetup     -> "setup"

instance FromJSON PlanJson where
    parseJSON = withObject "PlanJson" $ \o ->
        PlanJson <$> o .: "cabal-version"
                 <*> o .: "cabal-lib-version"
                 <*> o .: "compiler-id"
                 <*> o .: "arch"
                 <*> o .: "os"
                 <*> (toMap =<< o .: "install-plan")
      where
        toMap pil = do
            let pim = M.fromList [ (piId pi',pi') | pi' <- pil ]
            unless (M.size pim == length pil) $
                fail "install-plan[] has duplicate ids"
            pure pim

instance FromJSON PlanItem where
    parseJSON = withObject "PlanItem" $ \o -> do
        mcomponents    <- o .:? "components"
        mcomponentname <- o .:? "component-name"
        ty             <- o .:  "type"
        mstyle         <- o .:? "style"

        piId     <- o .: "id"
        piPId    <- PkgId <$> o .: "pkg-name" <*> o .: "pkg-version"
        piType   <- case (ty :: Text, mstyle :: Maybe Text) of
                    ("pre-existing",Nothing)      -> pure PIBuiltin
                    ("configured",Just "global")  -> pure PIGlobal
                    ("configured",Just "local")   -> pure PILocal
                    ("configured",Just "inplace") -> pure PILocal2
                    _                             -> fail (show (ty,mstyle))
        piFlags  <- o .:?! "flags"
        piSha256 <- o .:? "pkg-src-sha256"
        piComps  <- case (mcomponents, mcomponentname) of
          (Just comps0, Nothing) ->
              pure comps0
          (Nothing, Just cname) ->
              M.singleton cname <$> parseJSON (Object o)
          (Nothing, Nothing) | piType == PIBuiltin ->
              M.singleton CompNameLib <$> parseJSON (Object o)
          _ -> fail (show o)

        pure PlanItem{..}

instance ToField PIType where
    toField = toField . go
      where
        go :: PIType -> Text
        go = \case
            PIGlobal  -> "global"
            PILocal   -> "local"
            PILocal2  -> "local2"
            PIBuiltin -> "builtin"

-- toComp :: Text -> CompName
-- toComp = fromMaybe undefined . parseCompName

(.:?!) :: (FromJSON a, Monoid a) => Object -> Text -> Parser a
o .:?! fld = o .:? fld .!= mempty
