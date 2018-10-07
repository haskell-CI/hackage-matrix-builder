{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Log ( Priority(..)
           , log
           , logDebug
           , logInfo
           , logNotice
           , logWarning
           , logError

           , logDebugShow
           ) where

import           Prelude.Local

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale)
import qualified Data.Time.Format as DT
import           System.IO
import           System.IO.Unsafe

data Priority = DEBUG
              | INFO
              | NOTICE
              | WARNING
              | ERROR
              -- | CRITICAL
              -- | ALERT
              -- | EMERGENCY
              deriving (Eq, Ord, Enum, Bounded, Show, Read)

{-# NOINLINE mutex #-}
mutex :: MVar ()
mutex = unsafePerformIO $ newMVar ()

log' :: MonadIO m => Priority -> Text -> m ()
log' !prio !msg = liftIO $ do
    now  <- getCurrentTime
    line <- evaluate (T.concat [fmtTS now, " *", T.pack (show prio), "* ", msg])
    withMVar mutex $ \() -> do
      T.hPutStrLn stdout line
      hFlush stdout
  where
    fmtTS now = T.pack (take 23 (DT.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" now) ++ "Z")


logDebug, logInfo, logNotice, logWarning, logError :: MonadIO m => Text -> m ()
logDebug   = log' DEBUG
logInfo    = log' INFO
logNotice  = log' NOTICE
logWarning = log' WARNING
logError   = log' ERROR

logDebugShow :: (Show s, MonadIO m) => s -> m ()
logDebugShow = logDebug . T.pack . show
