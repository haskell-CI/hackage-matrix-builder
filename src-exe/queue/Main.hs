{-# LANGUAGE RecordWildCards #-}

import           Data.String.ToString
import qualified Queue

main :: IO ()
main = do
    q <- Queue.list
    case q of
        [] -> return ()
        (Queue.QueueItem {..}:_) -> putStrLn (toString qPackageName)
