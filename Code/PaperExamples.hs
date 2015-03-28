module Main where

import Feldspar
import qualified Feldspar.Stream as New
import qualified StreamOld as Old
import qualified IMonadicStream as Ix
import Feldspar.Vector as V
import Feldspar.SimpleVector
import Feldspar.Compiler

-- comp :: (SyntacticFeld prog) => FilePath -> String -> prog -> IO ()
comp fp fun prog = compile prog fp fun defaultOptions{useNativeArrays=True}

movAvgOld = Old.streamAsVector (Old.movingAvg 8) -:: newLen1 32 >-> tPull1 tDouble
movAvgNew = New.streamAsVector (New.movingAvg 8) -:: newLen1 32 >-> tPull1 tDouble

movingAverageExample :: IO ()
movingAverageExample = comp "movingAverageListing" "movingAvg" $
    ((New.streamAsVector (New.movingAvg 16)) -:: newLen1 16 . tPull1 tDouble >-> id)

main :: IO ()
main = do
  movingAverageExample
  return ()
