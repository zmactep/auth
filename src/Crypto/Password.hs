module Crypto.Password
    ( md5

    ) where

import qualified Data.ByteString.Char8 as B (pack)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack, unpack)
import Crypto.Hash (hash, Digest, MD5)

md5 :: Text -> Text
md5 x = pack $ show (hash (b2t x) :: Digest MD5)
    where b2t = B.pack . unpack
