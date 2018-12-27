{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Controller.Types where

import           Prelude.Local

import           Data.Pool
import qualified Database.PostgreSQL.Simple as PGS

import           PkgId
import           PkgIdxTsSet                (PkgIdxTsSet)

-- | See 'fetchPkgLstCache'
data PkgLstCache = PkgLstCache !PkgIdxTs !(Vector PkgN)

data App = App
  { appDbPool        :: Pool PGS.Connection
  , appPkgLstCache   :: MVar PkgLstCache   -- ^ see 'fetchPkgLstCache'
  , appPkgIdxTsCache :: MVar PkgIdxTsSet -- ^ see 'fetchPkgIdxTsCache'
  }

newtype ETag = ETag ByteString
             deriving (Eq,NFData)
