{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Schema
    ( User (..)
    , Group (..)
    , Session (..)
    , Bsonable (..), Default (..)
    , Login, Password, Name, Token, Title, Expires
    , UID, GID
    , uid, login, name, password, groups
    , gid, title
    , suid, token, expires
    , defaultTime
    ) where

import           Data.Bson.Serialization (Bsonable (..), mkBsonables)
import           Data.Default            (Default (..))
import           Data.Label              (mkLabels)
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

data User = User { _uid      :: UID
                 , _login    :: Login
                 , _name     :: Name
                 , _password :: Password
                 , _groups   :: [Integer]
                 }
    deriving (Show, Read, Eq, Ord)

data Group = Group { _gid   :: GID
                   , _title :: Title
                   }
    deriving (Show, Read, Eq, Ord)

data Session = Session { _suid    :: UID
                       , _token   :: Token
                       , _expires :: Expires
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
mkLabels [''User, ''Group, ''Session]
