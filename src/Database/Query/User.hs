{-# LANGUAGE OverloadedStrings #-}

module Database.Query.User
    ( addUser, checkLogin, updateUser
    , getAllUsers
    , getUserById
    , getUserByLogin, getUsersByLoginLike
    , getUsersByGroup
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad             (void)
import           Control.Monad.Trans       (liftIO)
import           Data.Monoid               ((<>))
import           Database.MongoDB          (Action, Collection, find, findOne,
                                            insert, modify, rest, select, (=:))
import           Database.Schema           (Bsonable (..), GID, Login, Password,
                                            UID, User (..))
import           Database.Schema.Selectors (groupSel, loginSel, passSel, uidSel)

usersCol :: Collection
usersCol = "users"

addUser :: User -> Action IO ()
addUser user = void <$> insert usersCol $ toBSON user

updateUser :: User -> Action IO ()
updateUser user = modify (select [uidSel $ uid user] usersCol) $ toBSON user

checkLogin :: Login -> Password -> Action IO (Maybe User)
checkLogin login' pass' = do let selectors = [loginSel login', passSel pass']
                             u <- findOne (select selectors usersCol)
                             sequence $ fromBSON <$> u

getAllUsers :: Action IO [User]
getAllUsers = do u <- find (select [] usersCol) >>= rest
                 sequence $ fromBSON <$> u

getUserById :: UID -> Action IO (Maybe User)
getUserById uid' = do u <- findOne (select [uidSel uid'] usersCol)
                      sequence $ fromBSON <$> u

getUserByLogin :: Login -> Action IO (Maybe User)
getUserByLogin login' = do u <- findOne (select [loginSel login'] usersCol)
                           sequence $ fromBSON <$> u

getUsersByLoginLike :: Login -> Action IO [User]
getUsersByLoginLike login' = do let loginLike = ".*" <> login' <> ".*"
                                u <- find (select [loginSel loginLike] usersCol) >>= rest
                                sequence $ fromBSON <$> u

getUsersByGroup :: GID -> Action IO [User]
getUsersByGroup gid' = do u <- find (select [groupSel gid'] usersCol) >>= rest
                          sequence $ fromBSON <$> u
