{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Api.User (resource) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson              (decode)
import           Data.Maybe
import           Data.Monoid
import           Data.String.Conversions
import qualified Data.Text               as T
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
    handler userName = do
      userInfo <- simpleHttp $ "http://hackage.haskell.org/user/" <> cs userName <> ".json"
      fmap hackageUserToUser . maybe (throwError NotFound) return . decode $ userInfo

hackageUserToUser :: HackageUserRep -> User
hackageUserToUser hu = User
  { uName     = huusername hu
  , uPackages = mapMaybe getPkg $ hugroups hu
  }
  where
    getPkg t = case T.splitOn "/" t of
      ["","package",pkgName,"maintainers"] -> Just . PackageName $ pkgName
      _                                    -> Nothing
