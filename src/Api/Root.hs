{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Api.Root
  ( ServerData (..)
  , Root (..)
  , runRoot
  , Db (..)
  ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.String.Conversions
import           Database.Persist.Sqlite
import           Happstack.Server
import           Happstack.Server.Monads      ()
import           Rest.Driver.Happstack        ()
import           Rest.Driver.Perform          (Rest)

import           Config                       (Config (sqliteDb),
                                               MonadConfig (..))

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
  newtype StM Root a = StMRoot { unStMRoot :: StM (ReaderT ServerData (ServerPartT IO)) a }
  liftBaseWith f = Root (liftBaseWith (\run -> f (liftM StMRoot . run . unRoot)))
  restoreM = Root . restoreM . unStMRoot

instance MonadConfig Root where
  asksConfig = asks . (. config)

class (MonadIO m, MonadConfig m) => Db m where
  runDb :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a

instance Db Root where
  runDb m = liftIO . flip runSqlite m =<< asksConfig  (cs . sqliteDb)

instance Db m => Db (ExceptT e m) where
  runDb = lift . runDb

instance Db m => Db (ReaderT r m) where
  runDb = lift . runDb
