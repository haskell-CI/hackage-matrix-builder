{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.String.Conversions
import           Data.String.ToString
import           Rest
import qualified Rest.Resource           as R
import           System.FilePath

import           Api.Package             (validatePackage)
import           Api.Root
import           Api.Types
import           Api.Utils
import           Config

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
      liftRoot (readReport pkgName) `orThrow` NotFound

readReport :: (MonadIO m, MonadReader ServerData m) => PackageName -> m (Maybe Report)
readReport pkgName = do
  repDir <- asks $ reportDir . config
  (fmap toReport . decode =<<) <$> liftIO (tryReadFile (cs repDir </> toString pkgName <.> "json"))
