{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerState
    ( ServerState (..)
    , DBConfig (..)
    , WebM
    , constructState
    , runIO
    ) where

import           Control.Exception    (try)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT,
                                       runReaderT)
import           Data.Pool            (Pool, createPool)
import           Database.MongoDB     (Database, Pipe, close, connect, host)
import           Network.Socket       (HostName)

data DBConfig = DBConfig { hostname :: HostName
                         , database :: Database
                         , timeout  :: Int
                         , stripes  :: Int
                         , rps      :: Int
                         }

data ServerState = ServerState { pool :: Pool Pipe
                               , db   :: Database
                               }

newtype WebM a = WebM { runWebM :: ReaderT ServerState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerState)

runIO :: ServerState -> WebM a -> IO a
runIO ss m = runReaderT (runWebM m) ss

constructState :: DBConfig -> IO ServerState
constructState sc = do pool <- createPool (connect (host hst)) close st to r
                       return $ ServerState pool db
    where hst = hostname sc
          db = database sc
          to = fromIntegral $ timeout sc
          st = stripes sc
          r  = rps sc
