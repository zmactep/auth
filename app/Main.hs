module Main where

import Control.Monad.Except (MonadIO, MonadError, catchError, liftIO)
import Control.Applicative  ((<$>))
import System.Environment   (getEnv)
import AuthServer           (runServer)

main :: IO ()
main = (read <$> getEnv "PORT" >>= runServer) `catchError` failMsg

failMsg :: (MonadError e m, MonadIO m, Show e) => e -> m ()
failMsg e = liftIO $ putStrLn ("Ooops: " ++ show e)
