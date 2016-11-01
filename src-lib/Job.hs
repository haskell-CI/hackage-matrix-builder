{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE StrictData        #-}

module Job
    ( JobStep(..), runStep
    , Task, newTask, runTask, cancelTask
      -- * internal utilities
    , runProc'
    ) where

import           Prelude.Local

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.Text.IO as T
import           System.IO
import qualified System.IO.Streams as Streams
-- import qualified System.IO.Streams.List as Streams
import           System.IO.Streams.Process
import           System.Process
import           Control.Concurrent.Async

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

    let cp = (proc exe args)
               { std_in    = UseHandle nullHandle
               , std_err   = UseHandle wend
               , std_out   = UseHandle wend
               , close_fds = True
               , create_group = True
               }

    (Nothing, Nothing, Nothing, ph) <- createProcess cp

    sOutErr <- Streams.handleToInputStream rend >>=
               Streams.atEndOfInput (hClose rend) >>=
               Streams.lockingInputStream

    return (sOutErr,ph)


ec2int :: ExitCode -> Int
ec2int ExitSuccess = 0
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
