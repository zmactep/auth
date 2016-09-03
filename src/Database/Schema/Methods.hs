{-# LANGUAGE OverloadedStrings #-}

module Database.Schema.Methods
    ( mkUser, updateLogin, updateName, updatePassword, addGroup, rmGroup
    , mkGroup, updateTitle
    ) where

import           Data.Label      (get, modify, set)
import           Data.List       (delete)
import           Data.Time       (NominalDiffTime, UTCTime, addUTCTime)
import           Database.Schema

mkUser :: Login -> Password -> UID -> User
mkUser login password uid = User { _uid = uid
                                 , _login = login
                                 , _password = password
                                 , _name = ""
                                 , _groups = []
                                 }

updateLogin :: Login -> User -> User
updateLogin = set login

updateName :: Name -> User -> User
updateName = set name

updatePassword :: Password -> User -> User
updatePassword = set password

addGroup :: GID -> User -> User
addGroup group user = modify groups mf user
    where mf = if group `elem` current then id else (group:)
          current = get groups user

rmGroup :: GID -> User -> User
rmGroup group = modify groups (delete group)

mkGroup :: Title -> GID -> Group
mkGroup title gid = Group { _gid = gid
                          , _title = title
                          }

updateTitle :: Title -> Group -> Group
updateTitle = set title
