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
import Clasterization

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
    let resultMatrix = run objectsList (epsilon configs) (numberOfCenters configs)
    print $ show resultMatrix