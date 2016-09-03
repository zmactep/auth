module Crypto.Session
    ( newSession
    ) where

import           Control.Applicative      ((<$>))
import           Crypto.Random            (getRandomBytes)
import           Data.ByteString          (ByteString, unpack)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Text                (pack)
import           Data.Time                (NominalDiffTime, UTCTime, addUTCTime,
                                           getCurrentTime)
import           Database.Schema          (Session (..), Token, UID)

sessionTimeout :: NominalDiffTime
sessionTimeout = 60 * 60 * 24 * 14

sessionExpiresFromNow :: IO UTCTime
sessionExpiresFromNow = addUTCTime sessionTimeout <$> getCurrentTime

genRandomToken :: IO Token
genRandomToken = do bytes <- getRandomBytes 64 :: IO ByteString
                    let chars = (w2c . (c2w 'A' +) . (`mod` 16)) <$> unpack bytes
                    return $ pack chars

newSession :: UID -> IO Session
newSession uid = do expires <- sessionExpiresFromNow
                    token <- genRandomToken
                    return $ Session uid token expires
