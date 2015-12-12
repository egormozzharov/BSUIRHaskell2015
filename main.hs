module Main where

import System.IO
import Data.Maybe

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.CSV.Conduit
import Data.Conduit

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow 
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
      

main = do
    let csvOpts = defCSVSettings {csvSep = (head (",")), csvQuoteChar = Nothing}  
    input <- runResourceT $ readCSVFile csvOpts "../lab1_fcm_clustering/butterfly.txt"

    let list = convertFromCsv input

    print $ show list