{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Schema
    ( User (..)
    , Group (..)
    , Session (..)
    , Bsonable (..), Default (..)
    , Login, Password, Name, Token, Title, Expires
    , UID, GID
    , defaultTime
    ) where

import           Data.Bson.Serialization (Bsonable (..), mkBsonables)
import           Data.Default            (Default (..))
import           Data.Text               (Text)
import           Data.Time               (UTCTime (..), fromGregorian)

type UID = Integer
type GID = Integer

type Login    = Text
type Password = Text
type Token    = Text
type Name     = Text
type Title    = Text

type Expires  = UTCTime

data User = User { uid      :: UID
                 , login    :: Login
                 , name     :: Name
                 , password :: Password
                 , groups   :: [Integer]
                 }
    deriving (Show, Read, Eq, Ord)

data Group = Group { gid   :: GID
                   , title :: Title
                   }
    deriving (Show, Read, Eq, Ord)

data Session = Session { suid    :: UID
                       , token   :: Token
                       , expires :: Expires
                       }
    deriving (Show, Read, Eq, Ord)

instance Default User where
  def = User 0 "" "" "" []

instance Default Group where
  def = Group 0 ""

instance Default UTCTime where
  def = defaultTime

defaultTime :: UTCTime
defaultTime = UTCTime (fromGregorian 1970 1 1) 0

instance Default Session where
  def = Session 0 "" def

mkBsonables [''User, ''Group, ''Session]
