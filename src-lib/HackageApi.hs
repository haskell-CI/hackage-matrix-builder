{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module HackageApi where

import           Prelude.Local

import qualified Data.Aeson               as J
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

type UserName = Text
type UserId = Word

data UserNameId = UserNameId
    { uniUsername :: UserName
    , uniUserid   :: UserId
    } deriving (Generic,Show)

instance ToJSON   UserNameId where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON UserNameId where { parseJSON = myParseJSON }

data UserInfo = UserInfo
    { uiGroups   :: Set Text
    , uiUsername :: UserName
    , uiUserid   :: UserId
    } deriving (Generic,Show)

instance ToJSON   UserInfo where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON UserInfo where { parseJSON = myParseJSON }




