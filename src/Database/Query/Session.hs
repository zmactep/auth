{-# LANGUAGE OverloadedStrings #-}

module Database.Query.Session
    ( addSession, delSession
    , getSession, getSessionByIdAndToken
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad             (void)
import           Data.Time                 (UTCTime)
import           Database.MongoDB          (Action, Collection, delete, find,
                                            findOne, insert, rest, select, (=:))
import           Database.Schema           (Bsonable (..), Session (..), Token,
                                            UID)
import           Database.Schema.Selectors (expiredSel, suidSel, tokenSel)

sessionCol :: Collection
sessionCol = "sessions"

cleanSessions :: UTCTime -> Action IO ()
cleanSessions now = delete (select [expiredSel now] sessionCol)

addSession :: Session -> Action IO ()
addSession session = void <$> insert sessionCol $ toBSON session

delSession :: Session -> Action IO ()
delSession session = do let suid  = _suid session
                        let token = _token session
                        let selectors = [suidSel suid, tokenSel token]
                        delete (select selectors sessionCol)

getSession :: Session -> Action IO (Maybe Session)
getSession session = do let suid  = _suid session
                        let token = _token session
                        getSessionByIdAndToken suid token

getSessionByIdAndToken :: UID -> Token -> Action IO (Maybe Session)
getSessionByIdAndToken suid token = do let selectors = [suidSel suid, tokenSel token]
                                       s <- findOne (select selectors sessionCol)
                                       sequence $ fromBSON <$> s
