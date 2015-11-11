{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Api.Package.Report.Cell (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List
import           Data.List.Split         (splitOn)
import           Data.String.Conversions
import           Rest
import qualified Rest.Resource           as R

import           Api.Package             (validatePackage)
import           Api.Package.Report      (WithReport, readReport)
import           Api.Types

data Cell = Cell
  { gvGhcVersion     :: VersionName
  , gvPackageVersion :: VersionName
  } deriving (Eq, Show)

toCell :: String -> Maybe Cell
toCell s =
  case splitOn "-" s of
    (a:b:_) -> Just Cell
      { gvGhcVersion     = cs a
      , gvPackageVersion = cs b
      }
    _          -> Nothing

type WithCell = ReaderT (Maybe Cell) WithReport

resource :: Resource WithReport WithCell (Maybe Cell) Void Void
resource = mkResourceReader
  { R.name   = "cell"
  , R.schema = noListing $ named [("id", singleBy toCell)]
  , R.get    = Just get
  }

get :: Handler WithCell
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithCell SingleResult
    handler = do
      pkgName   <- lift . lift . lift $ ask
      cell <- ask `orThrow` NotFound
      validatePackage pkgName
      report <- readReport pkgName `orThrow` NotFound
      return (byGhcVersion cell report) `orThrow` NotFound

byGhcVersion :: Cell -> Report -> Maybe SingleResult
byGhcVersion cell
  = fmap (toSingleResult $ gvPackageVersion cell)
  . find ((== gvGhcVersion cell) . ghcVersion)
  . rResults

toSingleResult :: VersionName -> GHCResult -> SingleResult
toSingleResult versionName gr = SingleResult
  { srGhcVersion     = ghcVersion gr
  , srGhcFullVersion = ghcFullVersion gr
  , srResultA       = find ((== versionName) . packageVersion) (resultsA gr)
  }
