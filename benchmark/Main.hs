{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Criterion.Main
import Criterion.Types
import Control.Exception (evaluate)

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal (new)
import Control.DeepSeq (NFData(..))

import Feldspar
import Feldspar.Vector
import Feldspar.Compiler (defaultOptions,Options(..),icompile)
import Feldspar.Compiler.Backend.C.Options (Platform(..))
import Feldspar.Compiler.Backend.C.Platforms (c99)
import Feldspar.Compiler.Plugin (loadFunOptsWith,pack)
import Feldspar.Compiler.Marshal

import Feldspar.Stream as New
import StreamOld as Old
import IMonadicStream as I

sizes :: [Length]
sizes = [2,3,4,5,8,9,15,32]

testdata :: [Double]
testdata = Prelude.cycle [1,2,3,4]

coeffs :: Length -> [Double]
coeffs n = [ 1 / fromIntegral i | i <- [1..n]]

copy_bench :: Pull1 Double -> Pull1 Double
copy_bench = id

fir_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
fir_bench cs v = New.streamAsVector (New.fir cs) v

fir2_bench :: [Data Double] -> Pull1 Double -> Pull1 Double
fir2_bench cs v = New.streamAsVector (New.fir2 cs) v

fir2_bench2  = fir2_bench (fmap value (coeffs 2))
fir2_bench3  = fir2_bench (fmap value (coeffs 3))
fir2_bench4  = fir2_bench (fmap value (coeffs 4))
fir2_bench5  = fir2_bench (fmap value (coeffs 5))
fir2_bench8  = fir2_bench (fmap value (coeffs 8))
fir2_bench9  = fir2_bench (fmap value (coeffs 9))
fir2_bench15 = fir2_bench (fmap value (coeffs 15))
fir2_bench32 = fir2_bench (fmap value (coeffs 32))

fir_old :: Pull1 Double -> Pull1 Double -> Pull1 Double
fir_old cs v = Old.streamAsVector (Old.fir cs) v

firI_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
firI_bench cs v = I.streamAsVector (I.fir cs) v

firI2_bench :: [Data Double] -> Pull1 Double -> Pull1 Double
firI2_bench cs v = New.streamAsVector (New.fir2 cs) v

firI2_bench2  = firI2_bench (fmap value (coeffs 2))
firI2_bench3  = firI2_bench (fmap value (coeffs 3))
firI2_bench4  = firI2_bench (fmap value (coeffs 4))
firI2_bench5  = firI2_bench (fmap value (coeffs 5))
firI2_bench8  = firI2_bench (fmap value (coeffs 8))
firI2_bench9  = firI2_bench (fmap value (coeffs 9))
firI2_bench15 = firI2_bench (fmap value (coeffs 15))
firI2_bench32 = firI2_bench (fmap value (coeffs 32))


mov_avg_bench :: Data Length -> Pull1 Double -> Pull1 Double
mov_avg_bench l = New.streamAsVector (New.movingAvg l)

mov_avg2_bench :: Length -> Pull1 Double -> Pull1 Double
mov_avg2_bench l = New.streamAsVector (New.movingAvg2 l)

mov_avg2_bench2  = mov_avg2_bench 2
mov_avg2_bench3  = mov_avg2_bench 3
mov_avg2_bench4  = mov_avg2_bench 4
mov_avg2_bench5  = mov_avg2_bench 5
mov_avg2_bench8  = mov_avg2_bench 8
mov_avg2_bench9  = mov_avg2_bench 9
mov_avg2_bench15 = mov_avg2_bench 15
mov_avg2_bench32 = mov_avg2_bench 32

mov_avg_old :: Data Length -> Pull1 Double -> Pull1 Double
mov_avg_old l = Old.streamAsVector (Old.movingAvg l)

loadFunOptsWith "" defaultOptions{platform=c99{values=[]}} ["-optc=-O3", "-optc=-save-temps"]
  [ 'copy_bench
  , 'fir_bench
  , 'fir_old
  , 'fir2_bench2
  , 'fir2_bench3
  , 'fir2_bench4
  , 'fir2_bench5
  , 'fir2_bench8
  , 'fir2_bench9
  , 'fir2_bench15
  , 'fir2_bench32
  , 'firI_bench
  , 'firI2_bench2
  , 'firI2_bench3
  , 'firI2_bench4
  , 'firI2_bench5
  , 'firI2_bench8
  , 'firI2_bench9
  , 'firI2_bench15
  , 'firI2_bench32
  , 'mov_avg_bench
  , 'mov_avg_old
  , 'mov_avg2_bench2
  , 'mov_avg2_bench3
  , 'mov_avg2_bench4
  , 'mov_avg2_bench5
  , 'mov_avg2_bench8
  , 'mov_avg2_bench9
  , 'mov_avg2_bench15
  , 'mov_avg2_bench32
  ]

fir2_benches =
  [ c_fir2_bench2_raw
  , c_fir2_bench3_raw
  , c_fir2_bench4_raw
  , c_fir2_bench5_raw
  , c_fir2_bench8_raw
  , c_fir2_bench9_raw
  , c_fir2_bench15_raw
  , c_fir2_bench32_raw
  ]

mov_avg2_benches =
  [ c_mov_avg2_bench2_raw
  , c_mov_avg2_bench3_raw
  , c_mov_avg2_bench4_raw
  , c_mov_avg2_bench5_raw
  , c_mov_avg2_bench8_raw
  , c_mov_avg2_bench9_raw
  , c_mov_avg2_bench15_raw
  , c_mov_avg2_bench32_raw
  ]

mkConfig :: FilePath -> Config
mkConfig report = defaultConfig { forceGC    = True
                                , reportFile = Just report
                                , csvFile    = Just "benchmark.csv"
                                }

setupPlugins :: IO ()
setupPlugins = do
  _ <- evaluate c_fir_bench_builder
  _ <- evaluate c_copy_bench_builder
  _ <- evaluate c_fir_old_builder
  _ <- evaluate c_fir2_bench2_builder
  _ <- evaluate c_fir2_bench3_builder
  _ <- evaluate c_fir2_bench4_builder
  _ <- evaluate c_fir2_bench5_builder
  _ <- evaluate c_fir2_bench8_builder
  _ <- evaluate c_fir2_bench9_builder
  _ <- evaluate c_fir2_bench15_builder
  _ <- evaluate c_fir2_bench32_builder
  _ <- evaluate c_mov_avg_bench_builder
  _ <- evaluate c_mov_avg_old_builder
  _ <- evaluate c_mov_avg2_bench2_builder
  _ <- evaluate c_mov_avg2_bench3_builder
  _ <- evaluate c_mov_avg2_bench4_builder
  _ <- evaluate c_mov_avg2_bench5_builder
  _ <- evaluate c_mov_avg2_bench8_builder
  _ <- evaluate c_mov_avg2_bench9_builder
  _ <- evaluate c_mov_avg2_bench15_builder
  _ <- evaluate c_mov_avg2_bench32_builder
  return ()

mkData ds l = evaluate =<< pack (Prelude.take (fromIntegral l) ds)

instance NFData (Ptr a)

setupData len = do
  d  <- mkData testdata 1024
  cs <- evaluate =<< pack (coeffs len)
  ds <- allocSA $ fromIntegral 1024 :: IO (Ptr (SA Double))
  o  <- new ds
  return (o,d,cs)

-- mkComp :: Length -> Benchmark
mkComp l b m = env (setupData l) $ \ ~(o,d,cs) -> bgroup (show l)
  [ bench "c_fir_ref"     (whnfIO $ c_fir_ref_raw cs d o)
  , bench "c_fir_bench"   (whnfIO $ c_fir_bench_raw cs d o)
  , bench "c_fir_old"     (whnfIO $ c_fir_old_raw cs d o)
  , bench "c_fir2_bench"  (whnfIO $ b d o)
  , bench "c_firI_bench"  (whnfIO $ c_firI_bench_raw cs d o)
  , bench "c_mov_avg_bench" (whnfIO $ c_mov_avg_bench_raw l d o)
  , bench "c_mov_avg_old"   (whnfIO $ c_mov_avg_old_raw l d o)
  , bench "c_mov_avg2_bench" (whnfIO $ m d o)
  ]

main :: IO ()
main = defaultMainWith (mkConfig "report.html")
  [ env setupPlugins $ \_ -> bgroup "fir" $ Prelude.zipWith3 mkComp sizes fir2_benches mov_avg2_benches
  ]

foreign import ccall unsafe "fir_ref.h fir_ref" c_fir_ref_raw
    :: Ptr (SA Double) -> Ptr (SA Double) -> Ptr (Ptr (SA Double)) -> IO ()

c_fir_ref :: [Double] -> [Double] -> IO [Double]
c_fir_ref cs xs = do
  cs1 <- pack cs
  xs1 <- pack xs
  ds <- allocSA $ fromIntegral (fromIntegral $ Prelude.length xs)
  o  <- new ds
  c_fir_ref_raw cs1 xs1 o
  peek o >>= from
