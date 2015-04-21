{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Package.Report (resource) where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson             (FromJSON (..), ToJSON (..), decode)
import qualified Data.ByteString.Lazy   as L
import           Data.JSON.Schema
import           Data.List
import qualified Data.Map.Strict        as Map
import           Data.String.ToString
import           Data.Text              (pack, unpack)
import qualified Data.Text              as T
import           Generics.Generic.Aeson
import           Rest
import           Rest.Info              (Info (..))
import qualified Rest.Resource          as R
import           Rest.ShowUrl
import           System.Directory
import           System.Directory

import           Api.Package            (Identifier (..), WithPackage)
import qualified Api.Package            as Package
import           Api.Types
import           BuildReport
import           BuildTypes

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
