module Main (main) where

import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen
import           Rest.Gen.Types

import           Api             (api)

main :: IO ()
main = do
  config <- Gen.configFromArgs "WebGen"
  Gen.generate config "Matrix" api
    [] -- Additional modules to put in the generated cabal file
    [] -- Additional imports in every module, typically used for orphan instances
    [(ModuleName "Data.Text.Internal", ModuleName "Data.Text")]
