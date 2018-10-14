{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Util.WebSvc
    (
      -- * 'FromJSON'/'ToJSON' instances
      ToJSON(..)
    , myToJSON, myToJSONCml
    , myToEncoding, myToEncodingCml
    , FromJSON(..)
    , myParseJSON, myParseJSONCml

      -- * Swagger instances
    , Swag.ToSchema(..)
    , myDeclareNamedSchema, myDeclareNamedSchemaCml
    ) where

import           Data.Char                       (isUpper, toLower)
import           GHC.Generics
import           Prelude

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Swagger                    as Swag
import qualified Data.Swagger.Declare            as Swag
import qualified Data.Swagger.Internal.Schema    as Swag
import qualified Data.Swagger.Internal.TypeShape as Swag


myDeclareNamedSchema, myDeclareNamedSchemaCml :: forall a proxy . (Generic a, Swag.GToSchema (Rep a), Swag.TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted") => proxy a -> Swag.Declare (Swag.Definitions Swag.Schema) Swag.NamedSchema
myDeclareNamedSchema = Swag.genericDeclareNamedSchema (Swag.defaultSchemaOptions { Swag.fieldLabelModifier = labelMod, Swag.constructorTagModifier = tagMod })
myDeclareNamedSchemaCml = Swag.genericDeclareNamedSchema (Swag.defaultSchemaOptions { Swag.fieldLabelModifier = labelModCml })

myToJSON, myToJSONCml :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
myToJSON = genericToJSON (defaultOptions { omitNothingFields = True, fieldLabelModifier = labelMod, constructorTagModifier = tagMod })
myToJSONCml = genericToJSON (defaultOptions { omitNothingFields = True, fieldLabelModifier = labelModCml })

myToEncoding, myToEncodingCml :: (Generic a, GToEncoding Zero (Rep a)) => a -> Encoding
myToEncoding = genericToEncoding (defaultOptions { omitNothingFields = True, fieldLabelModifier = labelMod, constructorTagModifier = tagMod })
myToEncodingCml = genericToEncoding (defaultOptions { omitNothingFields = True, fieldLabelModifier = labelModCml })

myParseJSON, myParseJSONCml :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
myParseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = labelMod, constructorTagModifier = tagMod })
myParseJSONCml = genericParseJSON (defaultOptions { fieldLabelModifier = labelModCml })

labelMod, tagMod, labelModCml :: String -> String
labelMod    = camelTo2 '_' . dropWhile (not . isUpper)
tagMod    = camelTo2 '_' . dropWhile isUpper
labelModCml = uncap        . dropWhile (not . isUpper)
  where
    uncap []     = []
    uncap (c:cs) = toLower c : cs

