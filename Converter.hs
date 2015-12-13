module Converter  
where  

import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit
import qualified Data.Vector as V
import qualified Data.Text as T

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow 
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])