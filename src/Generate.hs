module Generate (defaultMain, jsMain) where


import qualified Rest.Gen           as Gen
import qualified Rest.Gen.Config    as Gen
import           Rest.Gen.Types
import           System.Environment

import           Api                (api)

defaultMain :: IO ()
defaultMain = do
  config <- Gen.configFromArgs "WebGen"
  Gen.generate config "Matrix" api
    [] -- Additional modules to put in the generated cabal file
    [] -- Additional imports in every module, typically used for orphan instances
    [(ModuleName "Data.Text.Internal", ModuleName "Data.Text")]

jsMain :: IO ()
jsMain = withArgs ["--javascript", "--target=ui/api.js"] defaultMain
