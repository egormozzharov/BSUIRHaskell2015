module Main where

import System.IO
import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit
import Data.Random.Extras
import Data.List
import qualified Data.Vector as V
import qualified Data.Text as T

import Distance


m = 2
centersAmount = 2

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow 
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
      
-------------------Initialization------------------------------------------------------
getInitialCenters :: [a] -> Int -> [a]
getInitialCenters objectsList n = take n objectsList  

-------------------BelongingMatrixCalculation------------------------------------------

matrixCellValueOnIteration :: [Double] -> [Double] -> [Double] -> Double
matrixCellValueOnIteration centerFromList currCenter currObject = 
    if isNaN func then 1 else func  
    where func = ((euclidDistance currObject currCenter) / (euclidDistance currObject centerFromList)) ** (2 / (m - 1))

matrixCellValue :: [[Double]] -> [Double] -> [Double] -> Double
matrixCellValue centersList currCenter currObject = 
    (foldl (\acc centerFromList -> acc + matrixCellValueOnIteration centerFromList currCenter currObject) 0 centersList) ** (-1)

getBeloningsCoeffsForObject :: [[Double]] -> [Double] -> [Double]
getBeloningsCoeffsForObject centersList currObject = 
    map (\currCenter -> matrixCellValue centersList currCenter currObject) centersList


getBeloningsMatrix :: [[Double]] -> [[Double]] -> [[Double]]
getBeloningsMatrix objectsList centersList =
    map (\currObject -> getBeloningsCoeffsForObject centersList currObject) objectsList

--------------------CentersCalculation---------------------------------------------------
foldListOfLists :: [[Double]] -> [Double]
foldListOfLists listOfLists@(x:xs) = 
    zipWith (-) (foldl (\acc list -> zipWith (\x y -> x + y) acc list) x listOfLists) x  

nominatorValue :: [[Double]] -> [Double] -> [Double]
nominatorValue objectsList currBeloningsCoeffsList = 
    foldListOfLists $ zipWith (\currBelongingCoeff currObject  -> map (\x -> x * (currBelongingCoeff ** 2)) currObject) currBeloningsCoeffsList objectsList

denominatorValue :: [Double] -> Double
denominatorValue currBeloningsCoeffsList = 
    foldl (\acc currBelongingCoeff -> acc + (currBelongingCoeff ** 2)) 0 currBeloningsCoeffsList

getCenter :: [[Double]] -> [Double] -> [Double]
getCenter objectsList currBeloningsCoeffsList =
    map (\x -> x / (denominatorValue currBeloningsCoeffsList)) $ nominatorValue objectsList currBeloningsCoeffsList

getCenters :: [[Double]] -> [[Double]] -> [[Double]]
getCenters objectsList beloningsMatrix = 
    map (\currBeloningsCoeffsList -> getCenter objectsList currBeloningsCoeffsList) $ transpose beloningsMatrix

--------------------Clasterization-------------------------------------------------------
calculateCurrCoeff :: [[Double]] -> [[Double]] -> Double
calculateCurrCoeff oldMatrix newMatrix = 
    maximum ( map abs (zipWith (-) oldList newList) )
    where oldList = concat oldMatrix
          newList = concat newMatrix

clasterizationFinished :: [[Double]] -> [[Double]] -> Double -> Bool
clasterizationFinished oldMatrix newMatrix eps = 
    if eps > currCoeff then True else False
    where currCoeff = calculateCurrCoeff oldMatrix newMatrix

runClasterization :: [[Double]] -> [[Double]] -> Double -> [[Double]]
runClasterization objectsList beloningsMatrix eps
  | clasterizationFinished beloningsMatrix nextBeloningsMatrix eps = nextBeloningsMatrix
  | otherwise = runClasterization objectsList nextBeloningsMatrix eps
  where nextBeloningsMatrix = getBeloningsMatrix objectsList $ getCenters objectsList beloningsMatrix

run :: [[Double]] -> [[Double]]
run objectsList = 
    runClasterization objectsList beloningsMatrix 4
    where beloningsMatrix = getBeloningsMatrix objectsList (getInitialCenters objectsList 2)

---------------------------------------------------------------------------------------------
main = do
    let csvOpts = defCSVSettings {csvSep = (head (",")), csvQuoteChar = Nothing}  
    input <- runResourceT $ readCSVFile csvOpts "../lab1_fcm_clustering/irises.txt"

    let objectsList = convertFromCsv input
    let centersList = getInitialCenters objectsList centersAmount
    let beloningsMatrix = getBeloningsMatrix objectsList centersList
    let resultMatrix = run objectsList
    print $ show resultMatrix