module Main where

import System.IO
import Data.Maybe

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.CSV.Conduit
import Data.Conduit

import Data.Random.Extras

m = 2
centersList = [[1.0,2.0], [2.0,3.0]]
currCenter = [1.0,2.0]
currObject = [3.0,4.0]

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow 
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
      
euclidDistance :: Floating c => [c] -> [c] -> c
euclidDistance a b = sqrt (sum (zipWith (\a b -> (a - b)**2) a b))

getInitialCenters :: [a] -> Int -> [a]
getInitialCenters xs n = take n xs 
 
matrixCellValueOnIteration :: [Double] -> [Double] -> [Double] -> Double
matrixCellValueOnIteration centerFromList currCenter currObject = 
    ((euclidDistance currObject currCenter) / (euclidDistance currObject centerFromList)) ** (2 / (m - 1))

matrixCellValue :: [[Double]] -> [Double] -> [Double] -> Double
matrixCellValue centersList currCenter currObject = 
    (foldl (\acc centerFromList -> acc + matrixCellValueOnIteration centerFromList currCenter currObject) 0 centersList) ** (-1)




main = do
    let csvOpts = defCSVSettings {csvSep = (head (",")), csvQuoteChar = Nothing}  
    input <- runResourceT $ readCSVFile csvOpts "../lab1_fcm_clustering/butterfly.txt"

    let list = convertFromCsv input

    print $ show list