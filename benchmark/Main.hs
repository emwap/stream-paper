{-# LANGUAGE TemplateHaskell #-}

module Main where

import Criterion.Main
import Criterion.Types
import Control.Exception (evaluate)

import Foreign.Ptr
import Foreign.Marshal (new)
import Control.DeepSeq (NFData(..))

import Feldspar
import Feldspar.Vector (Pull1,thawPull1)
import Feldspar.Stream
import Feldspar.Compiler (defaultOptions,Options(..))
import Feldspar.Compiler.Backend.C.Options (Platform(..))
import Feldspar.Compiler.Backend.C.Platforms (c99)
import Feldspar.Compiler.Plugin (loadFunOptsWith,pack)
import Feldspar.Compiler.Marshal

sizes :: [Length]
sizes = [8,16,1024]

coeffs :: Pull1 Float
coeffs = thawPull1 $ value [1,0.5,0.25,0.125]

testdata :: [Float]
testdata = Prelude.cycle [1,2,3,4]

fir_bench :: Pull1 Float -> Pull1 Float
fir_bench = streamAsVector (fir coeffs)

loadFunOptsWith "" defaultOptions{platform=c99{values=[]}} ["-optc=-O2"] 'fir_bench

mkConfig :: FilePath -> Config
mkConfig report = defaultConfig { forceGC    = True
                                , reportFile = Just report
                                }

setupPlugins :: IO ()
setupPlugins = do
  _ <- evaluate c_fir_bench_builder
  return ()

mkData ds l = do
  evaluate =<< pack ([l], Prelude.take (fromIntegral l) ds)

instance NFData (Ptr a)

setupData len = do
  d  <- mkData testdata len
  ls <- pack [len]
  ds <- allocSA $ fromIntegral len :: IO (Ptr (SA Float))
  o  <- new (ls,ds)
  return (o,d)

mkComp :: Length -> Benchmark
mkComp l = env (setupData l) $ \ ~(o,d) ->
  mkBench "c_fir_bench" l (whnfIO $ c_fir_bench_raw d o)

mkBench name l = bench (name ++ "_" ++ show l)


main :: IO ()
main = defaultMainWith (mkConfig "fir_report.html")
  [ env setupPlugins $ \_ -> bgroup "compiled" $ Prelude.map mkComp sizes ]


