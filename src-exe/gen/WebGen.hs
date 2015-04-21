module Main (main, defaultMain) where

import qualified Rest.Gen           as Gen
import qualified Rest.Gen.Config    as Gen
import           Rest.Gen.Types
import           System.Environment

import           Api                (api)

main :: IO ()
main = do
  config <- Gen.configFromArgs "WebGen"
  Gen.generate config "Matrix" api
    [] -- Additional modules to put in the generated cabal file
    [] -- Additional imports in every module, typically used for orphan instances
    [(ModuleName "Data.Text.Internal", ModuleName "Data.Text")]

defaultMain :: IO ()
defaultMain = withArgs ["--javascript", "--target=ui/api.js"] main
