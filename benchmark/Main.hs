{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Criterion.Main
import Criterion.Types
import Control.Exception (evaluate)

import Foreign.Ptr
import Foreign.Marshal (new)
import Control.DeepSeq (NFData(..))

import qualified Data.Map as Map

import Feldspar
import Feldspar.Vector (Pull1,thawPull1)
import Feldspar.Compiler (defaultOptions,Options(..))
import Feldspar.Compiler.Backend.C.Options (Platform(..))
import Feldspar.Compiler.Backend.C.Platforms (c99)
import Feldspar.Compiler.Plugin (loadFunOptsWith,pack)
import Feldspar.Compiler.Marshal

import Feldspar.Stream as New
import StreamOld as Old

sizes :: [Length]
sizes = [8,16,1024,4096]

coeffs :: Pull1 Double
coeffs = thawPull1 $ value [1,0.5,0.25,0.125]

testdata :: [Double]
testdata = Prelude.cycle [1,2,3,4]

copy_bench :: Pull1 Double -> Pull1 Double
copy_bench = id

fir_bench :: Pull1 Double -> Pull1 Double
fir_bench v = share coeffs $ \cs -> New.streamAsVector (New.fir cs) v

fir_old :: Pull1 Double -> Pull1 Double
fir_old v = share coeffs $ \cs -> Old.streamAsVector (Old.fir cs) v

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
  d  <- mkData testdata len
  ls <- pack [len]
  ds <- allocSA $ fromIntegral len :: IO (Ptr (SA Double))
  o  <- new ds
  return (o,d)

mkComp :: Length -> Benchmark
mkComp l = env (setupData l) $ \ ~(o,d) -> bgroup (show l)
  [ bench "c_copy_bench"  (whnfIO $ c_copy_bench_raw d o)
  , bench "c_fir_bench"   (whnfIO $ c_fir_bench_raw d o)
  , bench "c_fir_old"     (whnfIO $ c_fir_old_raw d o)
  , bench "c_fir_ref"     (whnfIO $ c_fir_ref l d o)
  ]

mkRef :: Length -> Benchmark
mkRef l = env (setupData l) $ \ ~(o,d) -> bgroup (show l) [ bench "c_fir_ref" (whnfIO $ c_fir_ref l d o) ]

regroup :: Map.Map Length [Benchmark] -> [Benchmark]
regroup m = [ bgroup (show l) bs | (l,bs) <- Map.assocs m ]

main :: IO ()
main = defaultMainWith (mkConfig "fir_report.html")
  [ env setupPlugins $ \_ -> bgroup "fir" $ Prelude.map mkComp sizes
  ]

foreign import ccall unsafe "fir_ref.h fir_ref" c_fir_ref
    :: Length -> Ptr (SA Double) -> Ptr (Ptr (SA Double)) -> IO ()

