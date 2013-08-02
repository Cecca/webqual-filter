module Main where

import Filter
import Hash
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Data.BloomFilter
import Data.BloomFilter.Easy
import Data.BloomFilter.Hash
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    case args of
        _ ->  putStrLn help >> exitFailure

createFilter :: Int -- ^ Expected size
             -> Double -- ^ False positive rate
             -> FilePath -- ^ The file containing filter data
             -> IO (Bloom BS.ByteString)
createFilter n f filterDataFile = do
    filterData <- BSCL.readFile filterDataFile
    let (numBits, numHashes) = suggestSizing n f
    let dataLines = map (Hash.hash64 . BSCL.toStrict) $ BSCL.lines filterData
    return $ fromListB (cheapHashes numHashes) numBits dataLines


help :: String
help = "webqual-filter: removes graph nodes that correspond to web pages\n" ++
       "that gave an error response code.\n\n" ++ 
       "USAGE:\n" ++
       "    webqual-filter filterFile numElements"

