{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.List as DL
import Text.Printf
import Criterion.Main
import Criterion.Types
import Control.Exception (evaluate)

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal (new)
import Control.DeepSeq (NFData(..))

import Feldspar
import Feldspar.Vector as V
import Feldspar.Mutable
import Feldspar.Compiler (defaultOptions,Options(..),icompile)
import Feldspar.Compiler.Backend.C.Options (Platform(..))
import Feldspar.Compiler.Backend.C.Platforms (c99)
import Feldspar.Compiler.Plugin (loadFunOptsWith,pack)
import Feldspar.Compiler.Marshal

import Feldspar.Stream as New
import StreamOld as Old
import IMonadicStream as I

sizes :: [Length]
sizes = [2,5,9,15,32,64,128]

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

-- fir3_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
-- fir3_bench cs v = New.streamAsVector (New.fir3 cs) v

fir2_bench2   = fir2_bench (fmap value (coeffs 2))
fir2_bench5   = fir2_bench (fmap value (coeffs 5))
fir2_bench9   = fir2_bench (fmap value (coeffs 9))
fir2_bench15  = fir2_bench (fmap value (coeffs 15))
fir2_bench32  = fir2_bench (fmap value (coeffs 32))
fir2_bench64  = fir2_bench (fmap value (coeffs 64))
fir2_bench128 = fir2_bench (fmap value (coeffs 128))

fir_old :: Pull1 Double -> Pull1 Double -> Pull1 Double
fir_old cs v = Old.streamAsVector (Old.fir cs) v

firI_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
firI_bench cs v = I.streamAsVector (I.fir cs) v

firI2_bench :: [Data Double] -> Pull1 Double -> Pull1 Double
firI2_bench cs v = I.streamAsVector (I.fir2 cs) v

firI2_bench2   = firI2_bench (fmap value (coeffs 2))
firI2_bench5   = firI2_bench (fmap value (coeffs 5))
firI2_bench9   = firI2_bench (fmap value (coeffs 9))
firI2_bench15  = firI2_bench (fmap value (coeffs 15))
firI2_bench32  = firI2_bench (fmap value (coeffs 32))
firI2_bench64  = firI2_bench (fmap value (coeffs 64))
firI2_bench128 = firI2_bench (fmap value (coeffs 128))

firI3_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
firI3_bench cs v = I.streamAsVector (I.firNoMod cs) v

mov_avg_bench :: Data Length -> Pull1 Double -> Pull1 Double
mov_avg_bench l = New.streamAsVector (New.movingAvg l)

mov_avg2_bench :: Length -> Pull1 Double -> Pull1 Double
mov_avg2_bench l = New.streamAsVector (New.movingAvg2 l)

mov_avg2_bench2  = mov_avg2_bench 2
mov_avg2_bench5  = mov_avg2_bench 5
mov_avg2_bench9  = mov_avg2_bench 9
mov_avg2_bench15 = mov_avg2_bench 15
mov_avg2_bench32 = mov_avg2_bench 32
mov_avg2_bench64 = mov_avg2_bench 64
mov_avg2_bench128 = mov_avg2_bench 128

mov_avg_old :: Data Length -> Pull1 Double -> Pull1 Double
mov_avg_old l = Old.streamAsVector (Old.movingAvg l)

loadFunOptsWith "" defaultOptions{platform=c99{values=[]}} ["-optc=-O3", "-optc=-save-temps"]
  [ 'copy_bench
  , 'fir_bench
  , 'fir_old
  , 'fir2_bench2
  , 'fir2_bench5
  , 'fir2_bench9
  , 'fir2_bench15
  , 'fir2_bench32
  , 'fir2_bench64
  , 'fir2_bench128
--   , 'fir3_bench
  , 'firI_bench
  , 'firI2_bench2
  , 'firI2_bench5
  , 'firI2_bench9
  , 'firI2_bench15
  , 'firI2_bench32
  , 'firI2_bench64
  , 'firI2_bench128
  , 'firI3_bench
  , 'mov_avg_bench
  , 'mov_avg_old
  , 'mov_avg2_bench2
  , 'mov_avg2_bench5
  , 'mov_avg2_bench9
  , 'mov_avg2_bench15
  , 'mov_avg2_bench32
  , 'mov_avg2_bench64
  , 'mov_avg2_bench128
  ]

fir2_benches =
  [ c_fir2_bench2_raw
  , c_fir2_bench5_raw
  , c_fir2_bench9_raw
  , c_fir2_bench15_raw
  , c_fir2_bench32_raw
  , c_fir2_bench64_raw
  , c_fir2_bench128_raw
  ]

firI2_benches =
  [ c_firI2_bench2_raw
  , c_firI2_bench5_raw
  , c_firI2_bench9_raw
  , c_firI2_bench15_raw
  , c_firI2_bench32_raw
  , c_firI2_bench64_raw
  , c_firI2_bench128_raw
  ]

mov_avg2_benches =
  [ c_mov_avg2_bench2_raw
  , c_mov_avg2_bench5_raw
  , c_mov_avg2_bench9_raw
  , c_mov_avg2_bench15_raw
  , c_mov_avg2_bench32_raw
  , c_mov_avg2_bench64_raw
  , c_mov_avg2_bench128_raw
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
  _ <- evaluate c_fir2_bench5_builder
  _ <- evaluate c_fir2_bench9_builder
  _ <- evaluate c_fir2_bench15_builder
  _ <- evaluate c_fir2_bench32_builder
  _ <- evaluate c_fir2_bench64_builder
  _ <- evaluate c_fir2_bench128_builder
--   _ <- evaluate c_fir3_bench_builder
  _ <- evaluate c_firI_bench_builder
  _ <- evaluate c_firI2_bench2_builder
  _ <- evaluate c_firI2_bench5_builder
  _ <- evaluate c_firI2_bench9_builder
  _ <- evaluate c_firI2_bench15_builder
  _ <- evaluate c_firI2_bench32_builder
  _ <- evaluate c_firI2_bench64_builder
  _ <- evaluate c_firI2_bench128_builder
  _ <- evaluate c_firI3_bench_builder
  _ <- evaluate c_mov_avg_bench_builder
  _ <- evaluate c_mov_avg_old_builder
  _ <- evaluate c_mov_avg2_bench2_builder
  _ <- evaluate c_mov_avg2_bench5_builder
  _ <- evaluate c_mov_avg2_bench9_builder
  _ <- evaluate c_mov_avg2_bench15_builder
  _ <- evaluate c_mov_avg2_bench32_builder
  _ <- evaluate c_mov_avg2_bench64_builder
  _ <- evaluate c_mov_avg2_bench128_builder
  return ()

mkData ds l = evaluate =<< pack (Prelude.take (fromIntegral l) ds)

instance NFData (Ptr a) where rnf !_ = ()

setupData len = do
  d  <- mkData testdata 1024
  cs <- evaluate =<< pack (coeffs len)
  ds <- allocSA $ fromIntegral 1024 :: IO (Ptr (SA Double))
  o  <- new ds
  return (o,d,cs)

-- mkComp :: Length -> Benchmark
mkFirComp l b b2 = env (setupData l) $ \ ~(o,d,cs) -> bgroup (show l)
  [ bench "c_fir_ref"     (whnfIO $ c_fir_ref_raw cs d o)
  , bench "c_fir_bench"   (whnfIO $ c_fir_bench_raw cs d o)
  , bench "c_fir_old"     (whnfIO $ c_fir_old_raw cs d o)
  , bench "c_fir2_bench"  (whnfIO $ b d o)
--   , bench "c_fir3_bench"  (whnfIO $ c_fir3_bench_raw cs d o)
  , bench "c_firI_bench"  (whnfIO $ c_firI_bench_raw cs d o)
  , bench "c_firI2_bench"  (whnfIO $ b2 d o)
  , bench "c_firI3_bench"  (whnfIO $ c_firI3_bench_raw cs d o)
  ]

mkAvgComp l m = env (setupData l) $ \ ~(o,d,cs) -> bgroup (show l)
  [ bench "c_mov_avg_bench" (whnfIO $ c_mov_avg_bench_raw l d o)
  , bench "c_mov_avg_old"   (whnfIO $ c_mov_avg_old_raw l d o)
  , bench "c_mov_avg2_bench" (whnfIO $ m d o)
  ]

main :: IO ()
main = defaultMainWith (mkConfig "report.html")
  [ env setupPlugins $ \_ -> bgroup "fir" $ DL.zipWith3 mkFirComp sizes fir2_benches firI2_benches
  , env setupPlugins $ \_ -> bgroup "avg" $ DL.zipWith  mkAvgComp sizes mov_avg2_benches
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
