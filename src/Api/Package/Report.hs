{-# LANGUAGE LambdaCase #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.String.ToString
import           Rest
import qualified Rest.Resource        as R
import           System.FilePath

import           Api.Package          (validatePackage)
import           Api.Types
import           Api.Utils

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
      validatePackage pkgName
      liftIO (get_ pkgName) `orThrow` NotFound

get_ :: PackageName -> IO (Maybe Report)
get_ pkgName = (fmap toReport . decode =<<) <$> tryReadFile ("report" </> toString pkgName <.> "json")
