{-# LANGUAGE OverloadedStrings #-}

module Database.Schema.Selectors
    ( uidSel, loginSel, passSel, groupSel
    , gidSel, titleSel
    , suidSel, tokenSel, expiredSel
    ) where

import           Data.Bson       (Field, Label, Val, label, (=:))
import           Data.Time       (UTCTime)
import           Database.Schema

-- User UID Login Name Password [GID]

labelN :: Bsonable a => a -> Int -> Label
labelN d = label . (toBSON d !!)

makeField :: (Val a, Bsonable b) => b -> Int -> a -> Field
makeField d n = (labelN d n =:)

uidSel :: UID -> Field
uidSel = makeField (def::User) 0

loginSel :: Login -> Field
loginSel = makeField (def::User) 1

passSel :: Password -> Field
passSel = makeField (def::User) 3

groupSel :: GID -> Field
groupSel gid = makeField (def::User) 4 ("$in" =: [gid])

-- Group GID Title

gidSel :: GID -> Field
gidSel = makeField (def::Group) 0

titleSel :: Title -> Field
titleSel = makeField (def::Group) 1

-- Session UID Token Expires

suidSel :: UID -> Field
suidSel = makeField (def::Session) 0

tokenSel :: Token -> Field
tokenSel = makeField (def::Session) 1

expiredSel :: UTCTime -> Field
expiredSel now = makeField (def::Session) 2 ("$lt" =: now)
