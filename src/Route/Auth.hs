{-# LANGUAGE OverloadedStrings #-}

module Route.Auth
    ( checkAccessR
    , whoAmIR
    , loginR
    , logoutR
    ) where

import           Control.Applicative    ((<$>))
import           Control.Monad          (MonadPlus, forM_, join, mfilter)
import           Control.Monad.Trans    (MonadIO, lift, liftIO)
import           Crypto.Password        (md5)
import           Crypto.Session         (newSession)
import           Data.Maybe             (fromJust, isJust)
import           Data.Text.Lazy         (Text, pack)
import           Data.Time              (NominalDiffTime, getCurrentTime)
import           Database.Query         (makeQuery)
import           Database.Query.Session (addSession, delSession, getSession)
import           Database.Query.User    (addUser, checkLogin, getUserById)
import           Database.Schema        (GID, Login, Password, Session (..),
                                         User (..), defaultTime)
import           Network.HTTP.Types     (forbidden403, ok200, unauthorized401)
import           Route.Auth.Cookies     (getCookie, setCookie)
import           ServerState            (WebM)
import           Web.Scotty.Trans       (ActionT, param, setHeader, status,
                                         text, rescue, next)

validateSession :: (MonadIO m, MonadPlus m) => Session -> m Session
validateSession s = do now <- liftIO getCurrentTime
                       let check = (> now) . expires
                       mfilter check $ return s

whoAmIR :: ActionT Text WebM ()
whoAmIR = do
    userMb <- getUserByCookies
    case userMb of
        Just user -> do
            status ok200
            text $ pack (show $ login user)
        Nothing -> status unauthorized401

checkAccessR :: ActionT Text WebM ()
checkAccessR = do -- check cookies for :group access, return 200 or 403
    groupId <- param "group" `rescue` const next :: ActionT Text WebM GID
    user <- getUserByCookies
    let userGroups = groups <$> user
    case mfilter (elem groupId) userGroups of
        Just _  -> status ok200
        Nothing ->
            if isJust user then status forbidden403
                           else status unauthorized401

loginR :: ActionT Text WebM ()
loginR = do -- check login and password, return 200 or 403
    login'    <- param "login"    :: ActionT Text WebM Login
    password' <- param "password" :: ActionT Text WebM Password
    dbUser <- makeQuery $ checkLogin login' (md5 password')
    sessMb <- liftIO . sequence $ newSession . uid <$> dbUser
    case sessMb of
        Just _  -> do
            makeQuery $ addSession (fromJust sessMb)
            setCookie (fromJust sessMb)
            status ok200
        Nothing -> status forbidden403

logoutR :: ActionT Text WebM ()
logoutR = do -- delete session by uid in cookies, return 200
    cookieSession <- getCookie
    forM_ cookieSession $ \session ->
        makeQuery $ delSession session
    status unauthorized401

getUserByCookies :: ActionT Text WebM (Maybe User)
getUserByCookies = do
    cookieSession <- getCookie
    dbSession <-  join <$> sequence (makeQuery . getSession <$> cookieSession)
    session <- sequence $ validateSession <$> dbSession
    join <$> sequence (makeQuery . getUserById . suid <$> session)
