{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Controller.Config where

import           Prelude.Local

import           Config.Schema
import           Data.Bits
import qualified Data.Text      as T
import           PkgId
import           Servant.Client.Core (BaseUrl (..), parseBaseUrl)

data CtrlConf = CtrlConf
    { ccPort    :: Word16
    , ccWorkers :: [WorkerCfg]
    } deriving (Show)

type WorkerCfg = (BaseUrl,CompilerID)

readConfig :: FilePath -> IO CtrlConf
readConfig = loadValueFromFile ctrlConfSpec
  where
    ctrlConfSpec :: ValueSpecs CtrlConf
    ctrlConfSpec = sectionsSpec "CtrlConf" $
        CtrlConf <$> reqSection' "port" portSpec "some docs"
                 <*> reqSection' "workers" (oneOrList workerSpec) "some docs"

    portSpec :: ValueSpecs Word16
    portSpec = sizedBitsSpec "port"

    -- FIXME: available in newer config-schema
    sizedBitsSpec :: (Integral a, Bits a) => Text -> ValueSpecs a
    sizedBitsSpec lbl = customSpec lbl (liftValueSpec IntegerSpec) toIntegralSized

    workerSpec :: ValueSpecs WorkerCfg
    workerSpec = sectionsSpec "worker" $
        (,) <$> reqSection' "url" urlSpec "some docs"
            <*> reqSection' "compiler-id" compilerSpec "some docs"

    urlSpec :: ValueSpecs BaseUrl
    urlSpec = customSpec "url" valuesSpec (parseBaseUrl . T.unpack)

    compilerSpec :: ValueSpecs CompilerID
    compilerSpec = customSpec "compiler-id" valuesSpec (simpleParse . T.unpack)
