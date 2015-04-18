{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Api.Types (ServerData (..), Root (..), runRoot) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Happstack.Server
import           Happstack.Server.Monads     ()
import           Rest.Driver.Happstack       ()
import           Rest.Driver.Perform         (Rest)

data ServerData = ServerData

newtype Root a = Root { unRoot :: ReaderT ServerData (ServerPartT IO) a }
  deriving
    ( Alternative
    , Applicative
    , FilterMonad Response
    , Functor
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
  askRqEnv = Root (lift askRqEnv)
  rqDataError = Root . lift . rqDataError
  localRqEnv f (Root (ReaderT m)) = Root (ReaderT (localRqEnv f . m))

runRoot :: ServerData -> Root a -> ServerPartT IO a
runRoot serverData = flip runReaderT serverData . unRoot

instance MonadBaseControl IO Root where
  newtype StM Root a = StMRoot { unStMRoot :: StM (ReaderT ServerData (ServerPartT IO)) a }
  liftBaseWith f = Root (liftBaseWith (\run -> f (liftM StMRoot . run . unRoot)))
  restoreM = Root . restoreM . unStMRoot
