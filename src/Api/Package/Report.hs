{-# LANGUAGE LambdaCase #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy  as L
import           Data.List.Split
import qualified Data.Map              as Map
import           Data.Text             (unpack)
import           Happstack.Server.Auth (basicAuth)
import           Rest
import qualified Rest.Resource         as R
import           System.Directory
import           System.IO

import           Api.Package           (PackageIdentifier (..), WithPackage)
import           Api.Types
import           Queue

data ReportIdentifier = Latest

type WithReport = ReaderT ReportIdentifier WithPackage

resource :: Resource WithPackage WithReport ReportIdentifier Void Void
resource = mkResourceReader
  { R.name   = "report"
  , R.schema = noListing $ named [("latest", single Latest)]
  , R.create = Just create
  , R.get    = Just get
  }

create :: Handler WithPackage
create = mkInputHandler jsonI handler
  where
    handler :: Priority -> ExceptT Reason_ WithPackage ()
    handler prio = do
      login <- fmap (splitOn "/") . liftIO . readFile $ "auth"
      case login of
        [u,p] -> do
          lift $ basicAuth "localhost" (Map.fromList [(u, p)]) $ return ()
          Name pkg <- ask
          liftIO $ Queue.addToQueue (unpack pkg) prio
        _ -> do
          liftIO $ hPutStrLn stderr "Failure reading auth file"
          throwError Busy

get :: Handler WithReport
get = mkConstHandler jsonO handler
  where
    handler :: ExceptT Reason_ WithReport ReportDataJson
    handler = do
      pn <- lift . lift $ ask
      ident <- ask
      case ident of
        Latest -> byName pn
    byName :: PackageIdentifier -> ExceptT Reason_ WithReport ReportDataJson
    byName (Name t) = do
      let fp = "report/" ++ unpack t ++ ".json"
      exists <- liftIO $ doesFileExist fp
      unless exists $ throwError NotFound
      f <- liftIO $ L.readFile fp
      maybe (throwError Busy) (return . reportDataJson) . decode $ f
