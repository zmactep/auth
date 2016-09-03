{-# LANGUAGE OverloadedStrings #-}

module Database.Schema.Methods
    ( mkUser, updateLogin, updateName, updatePassword, addGroup, rmGroup
    , mkGroup, updateTitle
    ) where

import           Data.List       (delete)
import           Data.Time       (NominalDiffTime, UTCTime, addUTCTime)
import           Database.Schema

mkUser :: Login -> Password -> UID -> User
mkUser login password uid = User { uid = uid
                                 , login = login
                                 , password = password
                                 , name = ""
                                 , groups = []
                                 }

updateLogin :: Login -> User -> User
updateLogin l user = user { login = l }

updateName :: Name -> User -> User
updateName n user = user { name = n }

updatePassword :: Password -> User -> User
updatePassword p user = user { password = p }

addGroup :: GID -> User -> User
addGroup group user = user { groups = mf current }
    where mf = if group `elem` current then id else (group:)
          current = groups user

rmGroup :: GID -> User -> User
rmGroup group user = user { groups = delete group (groups user) }

mkGroup :: Title -> GID -> Group
mkGroup title gid = Group { gid = gid
                          , title = title
                          }

updateTitle :: Title -> Group -> Group
updateTitle t user = user { title = t }
