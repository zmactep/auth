{-# LANGUAGE OverloadedStrings #-}

module Database.Query.Group
    ( addGroup

    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (void)
import           Database.MongoDB    (Action, Collection, find, findOne, insert,
                                      rest, select, (=:))
import           Database.Schema     (Bsonable (..), Group (..), GID, Title)

groupsCol :: Collection
groupsCol = "groups"

addGroup :: Group -> Action IO ()
addGroup group = void <$> insert groupsCol $ toBSON group

getGroupById :: GID -> Action IO (Maybe Group)
getGroupById = undefined

getGroupByTitle :: Title -> Action IO (Maybe Group)
getGroupByTitle = undefined

getGroupsByTitleLike :: Title -> Action IO [Group]
getGroupsByTitleLike = undefined
