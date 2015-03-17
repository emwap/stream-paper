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
import Feldspar.Compiler (defaultOptions,Options(..))
import Feldspar.Compiler.Backend.C.Options (Platform(..))
import Feldspar.Compiler.Backend.C.Platforms (c99)
import Feldspar.Compiler.Plugin (loadFunOptsWith,pack)
import Feldspar.Compiler.Marshal

import Feldspar.Stream as New
import StreamOld as Old

sizes :: [Length]
sizes = [2,3,4,5,8,9]

testdata :: [Double]
testdata = Prelude.cycle [1,2,3,4]

coeffs :: Length -> [Double]
coeffs n = [ 1 / fromIntegral i | i <- [1..n]]

copy_bench :: Pull1 Double -> Pull1 Double
copy_bench = id

fir_bench :: Pull1 Double -> Pull1 Double -> Pull1 Double
fir_bench cs v = New.streamAsVector (New.fir cs) v

fir_old :: Pull1 Double -> Pull1 Double -> Pull1 Double
fir_old cs v = Old.streamAsVector (Old.fir cs) v

loadFunOptsWith "" defaultOptions{platform=c99{values=[]}} ["-optc=-O3", "-optc=-save-temps"]
  ['copy_bench, 'fir_bench, 'fir_old]

mkConfig :: FilePath -> Config
mkConfig report = defaultConfig { forceGC    = True
                                , reportFile = Just report
                                }

setupPlugins :: IO ()
setupPlugins = do
  _ <- evaluate c_fir_bench_builder
  _ <- evaluate c_copy_bench_builder
  _ <- evaluate c_fir_old_builder
  return ()

mkData ds l = evaluate =<< pack (Prelude.take (fromIntegral l) ds)

instance NFData (Ptr a)

setupData len = do
  d  <- mkData testdata 1024
  cs <- evaluate =<< pack (coeffs len)
  ds <- allocSA $ fromIntegral 1024 :: IO (Ptr (SA Double))
  o  <- new ds
  return (o,d,cs)

mkComp :: Length -> Benchmark
mkComp l = env (setupData l) $ \ ~(o,d,cs) -> bgroup (show l)
  [ bench "c_fir_ref"     (whnfIO $ c_fir_ref_raw cs d o)
  , bench "c_fir_bench"   (whnfIO $ c_fir_bench_raw cs d o)
  , bench "c_fir_old"     (whnfIO $ c_fir_old_raw cs d o)
  ]

main :: IO ()
main = defaultMainWith (mkConfig "fir_report.html")
  [ env setupPlugins $ \_ -> bgroup "fir" $ Prelude.map mkComp sizes
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
