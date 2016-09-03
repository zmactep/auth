{-# LANGUAGE OverloadedStrings #-}

module Route.Auth.Cookies
    ( setCookie
    , getCookie
    ) where

import           Control.Monad.Trans        (MonadIO)
import           Data.ByteString.Builder    (toLazyByteString)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.ByteString.Lazy.Char8 (toChunks)
import           Data.Default               (Default (..))
import           Data.Label                 (get, set)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Lazy             (Text)
import           Data.Text.Lazy.Encoding    (decodeUtf8, encodeUtf8)
import           Database.Schema            (Default (..), Session (..), UID,
                                             expires, suid, token)
import           Web.Cookie
import           Web.Scotty.Trans           (ActionT, header, setHeader)

getCookie :: MonadIO m => ActionT Text m (Maybe Session)
getCookie = do cookie <- header "Cookie"
               let session = (parseCookiesText . t2bs <$> cookie) >>= readCookie
               return session
   where t2bs = B.concat . toChunks . encodeUtf8

setCookie :: Monad m => Session -> ActionT e m ()
setCookie = setHeader "Set-Cookie" . renderCookie . mkCookie

session2value :: Session -> ByteString
session2value s = suidBS <> "-" <> tokenBS
    where suidBS  = B.pack $ show (get suid s)
          tokenBS = B.pack $ T.unpack (get token s)

readCookie :: CookiesText -> Maybe Session
readCookie [("SID", value)] = Just $ Session suid' token' def
    where [suidBS, token'] = T.split (== '-') value
          suid' = (read $ T.unpack suidBS) :: UID
readCookie _                = Nothing

mkCookie :: Session -> SetCookie
mkCookie session = def { setCookieName = "SID"
                       , setCookieValue = session2value session
                       , setCookieSecure = True
                       , setCookieExpires = Just $ get expires session
                       }

renderCookie :: SetCookie -> Text
renderCookie = decodeUtf8 . toLazyByteString . renderSetCookie
