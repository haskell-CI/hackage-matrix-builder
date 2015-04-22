{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text            as T
import           Rest
import qualified Rest.Resource        as R
import           System.Directory

import           Api.Package          (Identifier (..), WithPackage)

resource :: Resource WithPackage WithPackage Void Void Void
resource = mkResourceId
  { R.name   = "resource"
  , R.schema = noListing $ named []
  , R.create = Just create
  }

create :: Handler WithPackage
create = mkInputHandler stringI handler
  where
    handler :: String -> ExceptT Reason_ WithPackage ()
    handler pass = do
      unless (pass == "1234") $ throwError NotAllowed
      Name pkgName <- ask
      liftIO $ do
        createDirectoryIfMissing True "queue"
        writeFile ("queue/" ++ T.unpack pkgName) ""
