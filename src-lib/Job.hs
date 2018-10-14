{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module Job
    ( JobStep(..), runStep
    , Task, newTask, runTask, cancelTask
      -- * internal utilities
    , runProc'
    ) where

import           Prelude.Local
import           Util.WebSvc

import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
-- import qualified Data.Text.IO as T
import           System.IO
import qualified System.IO.Streams         as Streams
-- import qualified System.IO.Streams.List as Streams
import           Control.Concurrent.Async
import qualified Data.Map.Strict           as Map
import           System.IO.Streams.Process
import           System.Process

data JobStep = JobStep
    { jsExitCode :: Int
    , jsLog      :: Text
    , jsStart    :: UTCTime
    , jsDuration :: NominalDiffTime
    } deriving (Eq,Generic,Show)

instance ToJSON   JobStep where { toJSON = myToJSON; toEncoding = myToEncoding }
instance FromJSON JobStep where { parseJSON = myParseJSON }

runStep :: FilePath -> [Text] -> IO JobStep
runStep exe args = do
    !jsStart <- getCurrentTime
    (jsLog,jsExitCode) <- runProc' exe args
    !jsDuration <- (`diffUTCTime` jsStart) <$> getCurrentTime
    pure $! JobStep{..}

runProc' :: FilePath -> [Text] -> IO (Text, Int)
runProc' exe args = do
    -- TODO: use exception safe variant
    (s,ph) <- runProc exe (map T.unpack args)
    !bs <- T.decodeUtf8 <$> is2bs s
    !rc <- ec2int <$> waitForProcess ph
    pure (bs,rc)

is2bs :: InputStream ByteString -> IO ByteString
is2bs s = mconcat <$> Streams.toList s

-- merges stdout/stderr
runProc :: FilePath -> [String] -> IO (InputStream ByteString, ProcessHandle)
runProc exe args = do
    (rend, wend) <- createPipe

    nullHandle <- openFile "/dev/null" ReadMode -- TODO: are filedescriptors properly closed on process termination?

    env' <- case args of
             ("fetch":_:_) -> do
               -- FIXME/TODO: temporary hack until there's `cabal new-fetch`
               env0 <- Map.fromList <$> getEnvironment
               let path_val = "/home/matrixbot/bin:/home/matrixbot/.local/bin:/opt/ghc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"
               pure $ Just (Map.toList (Map.insert "PATH" path_val env0))

             _ -> pure Nothing

    let cp = (proc exe args)
               { std_in    = UseHandle nullHandle
               , std_err   = UseHandle wend
               , std_out   = UseHandle wend
               , close_fds = True
               , create_group = True
               , env       = env'
               }

    (Nothing, Nothing, Nothing, ph) <- createProcess cp

    sOutErr <- Streams.handleToInputStream rend >>=
               Streams.atEndOfInput (hClose rend) >>=
               Streams.lockingInputStream

    return (sOutErr,ph)


ec2int :: ExitCode -> Int
ec2int ExitSuccess     = 0
ec2int (ExitFailure i) = i


data Task a = TaskReady | TaskRunning (Async a)

runTask :: MVar (Task a) -> IO a -> IO a
runTask task go = do
    act <- modifyMVar task $ \case
        TaskReady -> do
            act' <- async go
            pure (TaskRunning act', act')
        TaskRunning act' ->
            pure (TaskRunning act', act')

    wait act

cancelTask :: MVar (Task a) -> IO ()
cancelTask task = do
    mact <- modifyMVar task $ \t0 -> case t0 of
        TaskReady -> pure (t0, Nothing)
        TaskRunning act' -> do
            pure (t0, Just act')

    maybe (pure ()) cancel mact


newTask :: IO (MVar (Task a))
newTask = newMVar TaskReady




{- default values used by 'proc'

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False,
                                delegate_ctlc = False,
                                detach_console = False,
                                create_new_console = False,
                                new_session = False,
                                child_group = Nothing,
                                child_user = Nothing }


-}
