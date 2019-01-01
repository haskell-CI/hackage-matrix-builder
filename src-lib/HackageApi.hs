{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module HackageApi where

import           Prelude.Local
import           Util.WebSvc

-- import qualified Data.Aeson               as J
import           Data.Swagger
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Servant.API
import           Servant.API.ContentTypes

data JSON0

instance Accept JSON0 where
    contentType _ = "application/json"

instance FromJSON a => MimeUnrender JSON0 a where
    mimeUnrender _ = eitherDecodeLenient

type HackageApi m =
    -- GET /users/
         "users" :> "" :> Get '[JSON0] [UserNameId]
    -- GET /user/:username
    :<|> "user"  :> Capture "username" UserName :> Get '[JSON0] UserInfo

type UserId = Word

data UserNameId = UserNameId
    { uniUsername :: UserName
    , uniUserid   :: UserId
    } deriving (Generic)

instance ToJSON   UserNameId where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON UserNameId where { parseJSON = myParseJSON }

data UserInfo = UserInfo
    { uiGroups   :: Set Text
    , uiUsername :: UserName
    , uiUserid   :: UserId
    } deriving (Generic)

instance ToJSON   UserInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON UserInfo where { parseJSON = myParseJSON }

----------------------------------------------------------------------------

newtype UserName = UserName Text
                 deriving (Show,Eq,Ord,Hashable,NFData,ToField,FromField,FromHttpApiData,ToHttpApiData)

instance ToSchema UserName where
    declareNamedSchema _ = pure $ NamedSchema (Just "UserName") $ mempty
        & type_ .~ SwaggerString
        & example ?~ toJSON (UserName "EdwardKmett")
        & description ?~ "Hackage Username"

instance ToParamSchema UserName where
    toParamSchema _ = mempty & type_ .~ SwaggerString

instance ToJSON UserName where
    toJSON (UserName t) = toJSON t
    toEncoding (UserName t) = toEncoding t

instance FromJSON UserName where
    parseJSON j = UserName <$> parseJSON j
