{-# LANGUAGE DeriveDataTypeable #-}

module Converter  
where  

import Prelude
import Numeric
import Control.Exception
import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit
import Data.List
import System.IO
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Text as T
import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS

data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile ::         FilePath
  ,outputFile :: FilePath
  ,numberOfCenters :: Int
  ,epsilon :: Double
  ,metrick :: Int
  ,header :: Bool
  ,rowNumber :: Bool 
  ,label :: Bool
  } deriving (Show, Data, Typeable)

convertFromCsv :: InputConfigs -> V.Vector (Row String) -> [[Double]]
convertFromCsv configs input = filter (not . null) $ processCsv $ V.toList input
    where processCsv rows = map processRow $ clearHeader rows
          clearHeader rows = if header configs then tail rows else rows
          processRow row = map (fromMaybe 0.0) $ filter isJust $ map maybeRead row
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
    
-- =====  output  =====

convertToCsv :: InputConfigs -> [[Double]] -> [B.ByteString]
convertToCsv configs = intersperse (BS.pack [13, 10]) . map (B.pack . intercalate (delemiter configs) . map ((\f -> f "") . showFFloat (Just 6)))

buildOutputHandle :: InputConfigs -> IO Handle
buildOutputHandle configs
    | outputFile configs /= ""  = handleAll (\e -> do { putStrLn $ "Cannot open file for output, using screen: " ++ show e; return stdout }) (openFile (outputFile configs) WriteMode)
    | otherwise            = return stdout

--------------------Exceptions---------------------------------------------------------------
handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle
---------------------------------------------------------------------------------------------

