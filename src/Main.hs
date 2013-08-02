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
        [filterFile,n,f,fileType,input,output] -> do
            putStrLn "Creating filter"
            bloom <- createFilter (read n) (read f) filterFile
            putStrLn "Created filter"
            case fileType of
                "urls" -> filterUrlsFile bloom input output
                "links" -> filterLinksFile bloom input output
                _ -> error "valid file types are only urls and links"
        _ ->  putStrLn help >> exitFailure


filterUrlsFile :: Bloom BS.ByteString -> FilePath -> FilePath -> IO ()
filterUrlsFile bloom input output = do
    BSCL.readFile input >>= return . filterUrls bloom >>= BSCL.writeFile output



filterLinksFile :: Bloom BS.ByteString -> FilePath -> FilePath -> IO ()
filterLinksFile = undefined


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
       "    webqual-filter filterFile numElements falsPositives fileType"++
                                                        " input output\n" ++
       "where filetype is one of the following:\n" ++
       "    links\n" ++
       "    urls\n"

