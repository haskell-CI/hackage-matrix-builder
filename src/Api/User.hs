{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.User (resource) where

import qualified Control.Exception       as E
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson              (decode)
import           Data.Maybe
import           Data.Monoid
import           Data.String.Conversions
import           Data.Text               (splitOn)
import           Network.HTTP.Conduit
import           Rest
import qualified Rest.Resource           as R

import           Api.Root
import           Api.Types

type WithUser = ReaderT Username Root

resource :: Resource Root WithUser Username Void Void
resource = mkResourceReader
  { R.name   = "user"
  , R.schema = noListing $ named [ ("name" , singleBy cs) ]
  , R.get    = Just get
  }

get :: Handler WithUser
get = mkIdHandler jsonO $ const handler
  where
    handler :: Username -> ExceptT Reason_ WithUser User
    handler username = do
      pkgs <- getUserPackages username
      return User
        { uName     = username
        , uPackages = pkgs
        }

getUserPackages :: Username -> ExceptT Reason_ WithUser [PackageName]
getUserPackages userName = do
  userInfo <- liftIO (getHackageUser userName) `orThrow` NotFound
  fmap (packagesInGroup . hugroups) . maybe (throwError NotFound) return . decode $ userInfo

getHackageUser :: Username -> IO (Maybe LazyByteString)
getHackageUser userName = (Just <$> simpleHttp ("http://hackage.haskell.org/user/" <> cs userName <> ".json"))
      `E.catch` (\StatusCodeException{} -> return Nothing)

packagesInGroup :: [StrictText] -> [PackageName]
packagesInGroup = mapMaybe getPkg
  where
    getPkg t = case splitOn "/" t of
      ["","package",pkgName,"maintainers"] -> Just . PackageName $ pkgName
      _                                    -> Nothing
