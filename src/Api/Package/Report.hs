{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List.Split
import qualified Data.Map              as Map
import qualified Data.Text             as T
import           Happstack.Server.Auth (basicAuth)
import           Rest
import qualified Rest.Resource         as R
import           System.Directory
import           System.IO

import           Api.Package           (Identifier (..), WithPackage)

resource :: Resource WithPackage WithPackage Void Void Void
resource = mkResourceId
  { R.name   = "resource"
  , R.schema = noListing $ named []
  , R.create = Just create
  }

create :: Handler WithPackage
create = mkConstHandler id handler
  where
    handler :: ExceptT Reason_ WithPackage ()
    handler = do
      login <- fmap (splitOn "/") . liftIO . readFile $ "auth"
      case login of
        [u,p] -> do
          lift $ basicAuth "localhost" (Map.fromList [(u, p)]) $ return ()
          Name pkgName <- ask
          liftIO $ do
            createDirectoryIfMissing True "queue"
            writeFile ("queue/" ++ T.unpack pkgName) ""
        _ -> do
          liftIO $ hPutStrLn stderr "Failure reading auth file"
          throwError Busy
