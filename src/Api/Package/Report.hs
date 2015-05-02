{-# LANGUAGE LambdaCase #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.String.ToString
import           Rest
import qualified Rest.Resource        as R
import           System.Directory
import           System.FilePath

import           Api.Types

data ReportIdentifier = Latest

type WithReport = ReaderT ReportIdentifier WithPackage

resource :: Resource WithPackage WithReport ReportIdentifier Void Void
resource = mkResourceReader
  { R.name   = "report"
  , R.schema = noListing $ named [("latest", single Latest)]
  , R.get    = Just get
  }

get :: Handler WithReport
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithReport Report
    handler = do
      pn <- lift . lift $ ask
      ident <- ask
      case ident of
        Latest -> byName pn
    byName :: PackageName -> ExceptT Reason_ WithReport Report
    byName pkgName = do
      let fp = "report" </> toString pkgName <.> "json"
      exists <- liftIO $ doesFileExist fp
      unless exists $ throwError NotFound
      f <- liftIO $ L.readFile fp
      maybe (throwError Busy) (return . toReport) . decode $ f
