module Filter (
    filterUrls,
    filterLinks
    ) where

import Hash
import Data.BloomFilter
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

filterUrls :: Bloom BSC.ByteString -- ^ The bloom filter containing invalid urls
           -> BSC.ByteString  -- ^ The input bytestring, to be split in lines
           -> BSC.ByteString
filterUrls bloomFilter = BSC.unlines . checkUrls . BSC.lines
    where checkUrls = filter (\l -> (hash64l . getUrl) l `elemB` bloomFilter)
          getUrl line = case BSC.split ' ' line of
                              [url] -> url
                              [_,url] -> url
                              _ -> error "Zero or more than two chunks"

-- | Links are expected to be encoded as 64 bit ID adjacency lists
filterLinks :: Bloom BS.ByteString -- ^ The bloom filter containing invalid urls
            -> BS.ByteString -- ^ The input bytestring, to be split in 
                            --   hash values
            -> BS.ByteString
filterLinks bloomFilter = unGroupHashes . checkLinks . groupHashes
    where checkLinks = filter (\l -> truncate64l l `elemB` bloomFilter)

-- | Given a bytestring, groups the bytes 8 by 8
groupHashes :: BS.ByteString -> [BS.ByteString]
groupHashes bs
    | BS.null bs = []
    | otherwise = BS.take 8 bs : groupHashes (BS.drop 8 bs)

-- | Concatenates a list of bytestrings into a single one
unGroupHashes :: [BS.ByteString] -> BS.ByteString
unGroupHashes = BS.concat


