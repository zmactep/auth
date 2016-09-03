{-# LANGUAGE OverloadedStrings #-}

module Route.Register
    (

    ) where

import           Data.Text.Lazy          (Text)
import           Database.Schema         (Login, Password)
import           Database.Schema.Methods (mkUser)
import           Network.HTTP.Types      (created201, forbidden403)
import           ServerState             (WebM)
import           Web.Scotty.Trans        (ActionT, param, setHeader, status)

register :: ActionT Text WebM ()
register = do
    login'    <- param "login"    :: ActionT Text WebM Login
    password' <- param "password" :: ActionT Text WebM Password
    let user = mkUser login' password' 1
    status created201
