module Hash (
    -- * Strict bytestrings
    hash64, truncate64, 
    -- * Lazy bytestrings
    hash64l, truncate64l
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Crypto.Hash
import Data.Bits

md5 :: BS.ByteString -> Digest MD5
{-# INLINE md5 #-}
md5 = hash

md5l :: BSL.ByteString -> Digest MD5
{-# INLINE md5l #-}
md5l = hashlazy

truncate64 :: BS.ByteString -> BS.ByteString
truncate64 = clearMSB . BS.take 8

truncate64l :: BSL.ByteString -> BSL.ByteString
truncate64l = BSL.fromStrict . truncate64 . BSL.toStrict

hash64 :: BS.ByteString -> BS.ByteString
hash64 = clearMSB . BS.take 8 . digestToByteString . md5

hash64l :: BSL.ByteString -> BSL.ByteString
hash64l = BSL.fromStrict . hash64 . BSL.toStrict

clearMSB :: BS.ByteString -> BS.ByteString
{-# INLINE clearMSB #-}
clearMSB bs 
    | BS.null bs = error "Cannot clear most significant bit of empty bytestring"
    | otherwise = clearBit first 7 `BS.cons` rest
    where Just (first, rest) = BS.uncons bs

