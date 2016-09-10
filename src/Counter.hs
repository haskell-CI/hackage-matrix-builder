{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Counter where

-- import Servant
import Servant.Docs
import Servant.HTML.Lucid
import Servant.Server (serve)
import Servant.Utils.Links
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar(..), readTVarIO, modifyTVar, newTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Monoid
import Data.Text (pack)
import GHC.Generics
import Lucid
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

newtype CounterVal = CounterVal { getCounterVal :: Int }
                     deriving (Show, FromJSON, ToJSON, Generic)

type GetCounter = Get '[JSON, HTML] CounterVal
type StepCounter = "step" :> Post '[JSON] ()
type SetCounter = ReqBody '[JSON] CounterVal :> Put '[JSON] ()
type Counter = GetCounter :<|> StepCounter :<|> SetCounter

counterAPI :: Proxy Counter
counterAPI = Proxy
-- | >>> putStrLn counterDocs
counterDocs :: String
counterDocs = markdown $ docs counterAPI

instance Num CounterVal where
  CounterVal x + CounterVal y = CounterVal (x + y)
  CounterVal x * CounterVal y = CounterVal (x * y)
  abs (CounterVal x) = CounterVal (abs x)
  signum (CounterVal x) = CounterVal (signum x)
  fromInteger x = CounterVal (fromInteger x)
  negate (CounterVal x) = CounterVal (negate x)

instance ToSample CounterVal where
  toSamples _ = [("First access",0),("After 1 step",1),("After 14 steps", 14)]

instance ToHtml CounterVal where
  toHtml (CounterVal val) =
    p_ ( toHtml
       $ "Current value: " ++ show val ++ "."
       )
    <> with form_ [action_ stepUrl, method_ "POST"]
                  (input_ [type_ "submit", value_ "Step!"])
    where
      stepUrl = pack $ show $ safeLink counterAPI (Proxy :: Proxy StepCounter)

  toHtmlRaw = toHtml

handleGetCounter :: TVar CounterVal -> Server GetCounter
handleGetCounter ctr = liftIO $ readTVarIO ctr

handleStepCounter :: TVar CounterVal -> Server StepCounter
handleStepCounter ctr = liftIO $ atomically $ modifyTVar ctr (+1)

handleSetCounter :: TVar CounterVal -> Server SetCounter
handleSetCounter ctr newValue = liftIO $ atomically $ writeTVar ctr newValue

handleCounter :: TVar CounterVal -> Server Counter
handleCounter ctr = handleGetCounter ctr
               :<|> handleStepCounter ctr
               :<|> handleSetCounter ctr
