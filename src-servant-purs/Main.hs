{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Prelude.Local

import           Language.PureScript.Bridge
import           Servant.PureScript
import Language.PureScript.Bridge.PSTypes
import WorkerApi

myBridge :: BridgePart
myBridge
   =  defaultBridge
  <|> (typeName ^== "Word" >> pure psInt)
  <|> (typeName ^== "CompilerID" >> pure psString)
  <|> (typeName ^== "PkgId" >> pure psString)
  <|> (typeName ^== "NoContent" >> pure psUnit)

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =  [
            mkSumType (Proxy :: Proxy WorkerInfo)
          , mkSumType (Proxy :: Proxy JobsInfo)
          , mkSumType (Proxy :: Proxy CreateJobReq)
          ]

mySettings :: Settings
mySettings = (addReaderParam "AuthToken" defaultSettings & apiModuleName .~ "WorkerApi2") {
  _generateSubscriberAPI = False
  }

main :: IO ()
main = do
  let frontEndRoot = "frontend/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy (Proxy :: Proxy (WorkerApi ()))
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
