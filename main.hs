{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import System.IO
import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit
import Data.Random.Extras
import Data.List
import System.Console.CmdArgs
import qualified Data.Vector as V
import qualified Data.Text as T

import Distance
import Converter


m = 2
     
-------------------Initialization------------------------------------------------------
getInitialCenters :: [a] -> Int -> [a]
getInitialCenters objectsList n = take n objectsList  

-------------------BelongingMatrixCalculation------------------------------------------

matrixCellValueOnIteration :: [Double] -> [Double] -> [Double] -> Double
matrixCellValueOnIteration centerFromList currCenter currObject = 
    if isNaN func then 1 else func  
    --where func = ((euclidDistance currObject currCenter) / (euclidDistance currObject centerFromList)) ** (2 / (m - 1))
    where func = ((hammingDistance currObject currCenter) / (hammingDistance currObject centerFromList)) ** (2 / (m - 1))

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

--------------------Clasterization----------------------------------------------------------
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
--------------------Exceptions---------------------------------------------------------------
handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle
---------------------------------------------------------------------------------------------
data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile :: FilePath
  ,outputFile :: FilePath
  ,numberOfCenters :: Int
  ,epsilon :: Double
  ,metrick :: Int
  ,header :: Bool
  ,rowNumber :: Bool
  ,label :: Bool
  } deriving (Show, Data, Typeable)

defaultInputConfigs = InputConfigs {
  delemiter = ","                                             &= help "Csv delemiter"
  ,inputFile = "../lab1_fcm_clustering/irises.txt"            &= help "Input file name"
  ,outputFile = ""                                            &= help "Output file name (default console)"
  ,numberOfCenters = 2                                        &= help "Clusters number"
  ,epsilon = 0.00001                                          &= help "Epsilon value"
  ,metrick = 0                                                &= help "Metrick: 0 - Hamming, 1 - Evclide"                    
  ,header = False                                             &= help "Have csv header?"
  ,rowNumber = False                                          &= help "Have csv number (row's head)?"
  ,label = False                                              &= help "Have csv class label (row's last)"
}                                                             &= summary "Lab1 FCM 2015" &= program "lab1"
--------------------------------------------------------------------------------------------

main = do
    configs <- cmdArgs defaultInputConfigs
    let csvOpts = defCSVSettings {csvSep = (head (",")), csvQuoteChar = Nothing}  
    input <- handleAll (\e -> error $ "Cannot read input file: " ++ show e ) $ runResourceT $ readCSVFile csvOpts $ inputFile configs

    let objectsList = convertFromCsv input
    let beloningsMatrix = getBeloningsMatrix objectsList (getInitialCenters objectsList (numberOfCenters configs))
    let resultMatrix = runClasterization objectsList beloningsMatrix (epsilon configs)
    print $ show resultMatrix