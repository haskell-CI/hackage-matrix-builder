module Api (api) where

import           Rest.Api

import qualified Api.Package as Package
import           Api.Types

api :: Api Root
api = [(mkVersion 1 0 0, Some1 router)]
  where
    router :: Router Root Root
    router =
      root -/ route package
    package = Package.resource
