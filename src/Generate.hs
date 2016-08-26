{-# LANGUAGE CPP #-}
module Generate (defaultMain, jsMain) where

import           Data.String.Conversions
import qualified Rest.Gen                as Gen
import qualified Rest.Gen.Config         as Gen
import           Rest.Gen.Types
import           System.Environment

import           Api                     (api)
import           Config

defaultMain :: IO ()
defaultMain = do
  config <- Gen.configFromArgs "WebGen"
  Gen.generate config "Matrix" api
    [] -- Additional modules to put in the generated cabal file
    [] -- Additional imports in every module, typically used for orphan instances
    [(mkModuleName "Data.Text.Internal", mkModuleName "Data.Text")]

#if MIN_VERSION_rest_gen(0,20,0)
mkModuleName :: String -> ModuleName ()
mkModuleName = ModuleName ()
#else
mkModuleName :: String -> ModuleName
mkModuleName = ModuleName
#endif

jsMain :: IO ()
jsMain = do
  jsPath <- jsClientTarget <$> defaultConfig
  withArgs ["--javascript", "--target=" ++ cs jsPath] defaultMain
