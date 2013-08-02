module Main where

import Filter
import Hash
{-import System.Environment (getArgs)-}
{-import qualified Data.ByteString.Lazy.Char8 as BSC-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Data.BloomFilter
import Data.BloomFilter.Easy
import Data.BloomFilter.Hash

main :: IO ()
main = undefined


createFilter :: Int -- ^ Expected size
             -> Double -- ^ False positive rate
             -> FilePath -- ^ The file containing filter data
             -> IO (Bloom BS.ByteString)
createFilter n f filterDataFile = do
    filterData <- BSCL.readFile filterDataFile
    let (numBits, numHashes) = suggestSizing n f
    let dataLines = map (Hash.hash64 . BSCL.toStrict) $ BSCL.lines filterData
    return $ fromListB (cheapHashes numHashes) numBits dataLines

