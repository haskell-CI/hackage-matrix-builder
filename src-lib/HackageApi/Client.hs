-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module HackageApi.Client where

import           Prelude.Local

import           Control.Monad.Except (ExceptT (..))
import           Servant.API
import           Servant.HttpStreams
import           Servant.Client.Core  (parseBaseUrl, ClientError)

import           HackageApi

runClientM' :: NFData a => BaseUrl -> ClientM a -> ExceptT ClientError IO a
runClientM' baseurl act = ExceptT $ withClientEnvIO baseurl (runClientM act)

getUsers    ::              ClientM [UserNameId]
getUserInfo :: UserName  -> ClientM UserInfo

getUsers :<|> getUserInfo = client hackageApi

hackageApi :: Proxy (HackageApi ())
hackageApi = Proxy

-- | Official Hackage URL
hackageUrl :: BaseUrl
hackageUrl = fromMaybe (error "the impossible happened") (parseBaseUrl "http://hackage-origin.haskell.org/")
