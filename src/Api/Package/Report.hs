{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Package.Report (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.JSON.Schema
import           Data.List.Split
import qualified Data.Map               as Map
import qualified Data.Text              as T
import           Generics.Generic.Aeson
import           GHC.Generics
import           Happstack.Server.Auth  (basicAuth)
import           Rest
import qualified Rest.Resource          as R
import           System.Directory
import           System.IO

import           Api.Package            (Identifier (..), WithPackage)

resource :: Resource WithPackage WithPackage Void Void Void
resource = mkResourceId
  { R.name   = "report"
  , R.schema = noListing $ named []
  , R.create = Just create
  }

data Priority
  = Low
  | Medium
  | High
  deriving (Eq, Generic, Show)

prioToString :: Priority -> String
prioToString = \case
  Low    -> "low"
  Medium -> "medium"
  High   -> "high"

instance ToJSON     Priority where toJSON    = gtoJson
instance FromJSON   Priority where parseJSON = gparseJson
instance JSONSchema Priority where schema    = gSchema

create :: Handler WithPackage
create = mkInputHandler jsonI handler
  where
    handler :: Priority -> ExceptT Reason_ WithPackage ()
    handler prio = do
      login <- fmap (splitOn "/") . liftIO . readFile $ "auth"
      case login of
        [u,p] -> do
          lift $ basicAuth "localhost" (Map.fromList [(u, p)]) $ return ()
          Name pkgName <- ask
          liftIO $ do
            createDirectoryIfMissing True "queue"
            writeFile ("queue/" ++ T.unpack pkgName) (prioToString prio)
        _ -> do
          liftIO $ hPutStrLn stderr "Failure reading auth file"
          throwError Busy
