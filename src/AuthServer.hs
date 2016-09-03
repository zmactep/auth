{-# LANGUAGE OverloadedStrings #-}

module AuthServer
    ( runServer
    , Port
    ) where

import           Control.Monad.Trans                  (lift)
import           Data.Text.Lazy                       (Text)
import           Network.HTTP.Types                   (forbidden403)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Route.Auth                           (checkAccess, login,
                                                       logout, whoAmI)
import           ServerState                          (DBConfig (..),
                                                       ServerState, WebM,
                                                       constructState, runIO)
import           Web.Scotty.Trans                     (ScottyT, defaultHandler,
                                                       get, middleware, post,
                                                       scottyT, status, text)

type Port = Int

config :: DBConfig
config = DBConfig "127.0.0.1" "auth" 300 1 5

runServer :: Port -> IO ()
runServer port = do state <- constructState config
                    scottyT port (runIO state) routes

routes :: ScottyT Text WebM ()
routes = do
    defaultHandler $ const $ status forbidden403 >> text "You have not be here"
    middleware logStdoutDev
    get  "/:gid"   checkAccess
    get  "/"       whoAmI
    post "/login"  login
    post "/logout" logout
