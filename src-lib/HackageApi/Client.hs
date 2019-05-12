-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module HackageApi.Client where

import           Prelude.Local

import           Control.Monad.Except (ExceptT (..))
import           Network.HTTP.Client  (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Client.Core  (parseBaseUrl)

import           HackageApi

runClientM' :: Manager -> BaseUrl -> ClientM a -> ExceptT ServantError IO a
runClientM' manager' baseurl act = ExceptT (runClientM act (ClientEnv manager' baseurl Nothing))

getUsers    ::              ClientM [UserNameId]
getUserInfo :: UserName  -> ClientM UserInfo

getUsers :<|> getUserInfo = client hackageApi

hackageApi :: Proxy (HackageApi ())
hackageApi = Proxy

-- | Official Hackage URL
hackageUrl :: BaseUrl
hackageUrl = fromMaybe (error "the impossible happened") (parseBaseUrl "http://hackage-origin.haskell.org/")
