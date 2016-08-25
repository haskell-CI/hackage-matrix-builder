{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Api.Root
  ( MonadRoot (..)
  , Root
  , ServerData (..)
  , runRoot
  ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Happstack.Server            (FilterMonad, Happstack,
                                              HasRqData (..), Response,
                                              ServerMonad, ServerPartT,
                                              WebMonad)
import           Happstack.Server.Monads     ()
import           Rest.Driver.Happstack       ()
import           Rest.Driver.Perform         (Rest)

import           Config                      (Config, MonadConfig (..))

data ServerData = ServerData { config :: Config }
  deriving Show

newtype Root a = Root { unRoot :: ReaderT ServerData (ServerPartT IO) a }
  deriving
    ( Applicative
    , Alternative
    , FilterMonad Response
    , Functor
    , Happstack
    , Monad
    , MonadBase IO
    , MonadIO
    , MonadPlus
    , MonadReader ServerData
    , Rest
    , ServerMonad
    , WebMonad Response
    )

instance HasRqData Root where
  askRqEnv                        = Root (lift askRqEnv)
  rqDataError                     = Root . lift . rqDataError
  localRqEnv f (Root (ReaderT m)) = Root (ReaderT (localRqEnv f . m))

runRoot :: ServerData -> Root a -> ServerPartT IO a
runRoot serverData = flip runReaderT serverData . unRoot

instance MonadBaseControl IO Root where
  type StM Root a = StM (ReaderT ServerData (ServerPartT IO)) a
  liftBaseWith f  = Root (liftBaseWith (\run -> f (run . unRoot)))
  restoreM        = Root . restoreM

instance MonadConfig Root where
  asksConfig = asks . (. config)

class (MonadIO m, MonadConfig m) => MonadRoot m where
  liftRoot :: Root a -> m a

instance MonadRoot Root where
  liftRoot = id

instance MonadRoot m => MonadRoot (ExceptT e m) where
  liftRoot = lift . liftRoot

instance MonadRoot m => MonadRoot (ReaderT r m) where
  liftRoot = lift . liftRoot
