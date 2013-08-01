module Filter (
    filterUrls,
    filterLinks,
    clearMSB
    ) where

import Data.BloomFilter
{-import Data.BloomFilter.Easy-}
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
{-import Converter-}
import Data.Bits

filterUrls :: Bloom BSC.ByteString -- ^ The bloom filter containing invalid urls
           -> BSC.ByteString  -- ^ The input bytestring, to be split in lines
           -> BSC.ByteString
filterUrls bloomFilter = BSC.unlines . checkUrls . BSC.lines
    where checkUrls = filter (`elemB` bloomFilter)

-- | Links are expected to be encoded as 64 bit ID adjacency lists
filterLinks :: Bloom BS.ByteString -- ^ The bloom filter containing invalid urls
            -> BS.ByteString -- ^ The input bytestring, to be split in 
                            --   hash values
            -> BS.ByteString
filterLinks bloomFilter = unGroupHashes . checkLinks . groupHashes
    where checkLinks = filter (\l -> clearMSB l `elemB` bloomFilter)

clearMSB :: BS.ByteString -> BS.ByteString
{-# INLINE clearMSB #-}
clearMSB bs 
    | BS.null bs = error "Cannot clear most significant bit of empty bytestring"
    | otherwise = clearBit first 7 `BS.cons` rest
    where Just (first, rest) = BS.uncons bs

-- | Given a bytestring, groups the bytes 16 by 16
groupHashes :: BS.ByteString -> [BS.ByteString]
groupHashes bs
    | BS.null bs = []
    | otherwise = BS.take 16 bs : groupHashes (BS.drop 16 bs)

-- | Concatenates a list of bytestrings into a single one
unGroupHashes :: [BS.ByteString] -> BS.ByteString
unGroupHashes = BS.concat


