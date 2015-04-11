module IMonadicStream where

import qualified Prelude as P
import qualified Control.Monad as P

import Feldspar
import Feldspar.Vector
          (Pull, Pull1, fromZero, toPull, arrToManifest
          ,freezePull1, indexed1, value1, fromList
          ,sum,length,replicate1,scalarProd)
import qualified Feldspar.Vector as V
import Feldspar.Vector.Shape (Shape(..),DIM1)
import Feldspar.Mutable
import qualified BufferNoMod as B

data Stream a = Stream (M (Data Index -> M a))

loop :: Monad m => a -> m a
loop = return

-- | Take the first element of a stream
head :: Syntax a => Stream a -> a
head (Stream init) = runMutable (init >>= ($ 0))

-- | 'map f str' transforms every element of the stream 'str' using the
--   function 'f'
map :: (a -> b) -> Stream a -> Stream b
map f (Stream init) = Stream $ fmap (\im i -> fmap f (im i)) init

recurrenceIO :: (Type a, Type b) =>
                Pull1 a -> Stream (Data a) -> Pull1 b ->
                (Pull1 a -> Pull1 b -> Data b) ->
                Stream (Data b)
recurrenceIO ii (Stream init) io mkExpr = Stream $ do
    next <- init
    ibuf <- initBuffer ii
    obuf <- initBuffer io
    loop $ \i -> do
      a <- next i
      whenM (lenI /= 0) $ putBuf ibuf a
      b <- withBuf ibuf $ \ib ->
             withBuf obuf $ \ob ->
               return $ mkExpr ib ob
      whenM (lenO /= 0) $ putBuf obuf b
      return b
  where
    lenI = length ii
    lenO = length io

recurrenceIONoMod :: (Type a, Type b) =>
                     Pull1 a -> Stream (Data a) -> Pull1 b ->
                     (Pull1 a -> Data Index -> Pull1 b -> Data Index -> Data b) ->
                     Stream (Data b)
recurrenceIONoMod ii (Stream init) io mkExpr = Stream $ do
    next <- init
    ibuf <- B.initBuffer ii
    obuf <- B.initBuffer io
    loop $ \i -> do
      a <- next i
      whenM (lenI /= 0) $ B.putBuf ibuf a
      b <- B.withBuf ibuf $ \i ib ->
             B.withBuf obuf $ \o ob ->
               return $ mkExpr ib i ob o
      whenM (lenO /= 0) $ B.putBuf obuf b
      return b
  where
    lenI = length ii
    lenO = length io

-- | A convenience function for translating an algorithm on streams to an algorithm on vectors.
--   The result vector will have the same length as the input vector.
--   It is important that the stream function doesn't drop any elements of
--   the input stream.
--
--   This function allocates memory for the output vector.
streamAsVector :: (Syntax a, Syntax b) =>
                  (Stream a -> Stream b)
               -> (Pull DIM1 a -> Pull DIM1 b)

streamAsVector f v = toPull $ arrToManifest (fromList [lv], take lv $ f $ unsafeVectorToStream v)
  where lv = length v

unsafeVectorToStream :: Syntax a => Pull DIM1 a -> Stream a
unsafeVectorToStream vec = Stream $ do
    loop $ \i -> do
      return (vec ! (Z :. i))

-- | 'take n str' allocates 'n' elements from the stream 'str' into a
--   core array.
take :: (Syntax a) => Data Length -> Stream a -> Data [Internal a]
take n (Stream init)
    = runMutableArray $ do
        marr <- newArr_ n
        next <- init
        forM n $ \ix -> do
          a <- next ix
          setArr marr ix (desugar a)
        return marr

cycle :: Syntax a => Pull DIM1 a -> Stream a
cycle vec = Stream $ do
    let l = length vec
    loop $ \i -> return (vec ! (Z:.(i `mod` l)))

recurrenceIO2 :: (Type a, Type b)
              => [Data a] -> Stream (Data a) -> [Data b] ->
                 ([Data a] -> [Data b] -> Data b) ->
                 Stream (Data b)
recurrenceIO2 ii (Stream init) io mkExpr = Stream $ do
    next <- init
    ris <- P.mapM newRef ii
    ros <- P.mapM newRef io
    loop $ \i -> do
      a <- next i
      if (P.not $ P.null ii) then pBuf ris a else return ()
      b <- wBuf ris $ \ib ->
             wBuf ros $ \ob ->
               return $ mkExpr ib ob
      if (P.not $ P.null io) then pBuf ros b else return ()
      return b
  where
    pBuf rs a = P.zipWithM (\r1 r2 -> getRef r1 >>= setRef r2) (P.tail $ P.reverse rs) (P.reverse rs) >> setRef (P.head rs) a
    wBuf rs f = P.mapM getRef rs >>= f

-- | A fir filter on streams
fir :: Numeric a => Pull1 a ->
       Stream (Data a) -> Stream (Data a)
fir b inp =
    recurrenceIO (replicate1 (length b) 0) inp (replicate1 1 0)
                 (\i _ -> scalarProd b i)

fir2 :: Numeric a => [Data a] -> Stream (Data a) -> Stream (Data a)
fir2 b inp =
  recurrenceIO2 (P.replicate (P.length b) 0) inp [] (\x _ -> P.sum $ P.zipWith (*) x b)

firNoMod  :: Numeric a => Pull1 a ->
             Stream (Data a) -> Stream (Data a)
firNoMod b inp =
  recurrenceIONoMod (replicate1 (length b) 0) inp (replicate1 1 0)
                    (\w i _ _ -> scalarProd (V.take (l-i) b) (V.drop i w)
                               + scalarProd (V.drop (l-i) b) (V.take i w))
  where l = length b

movingAvg :: (Fraction a, RealFloat a)
          => Data WordN -> Stream (Data a) -> Stream (Data a)
movingAvg n str = recurrenceIO (replicate1 n 0) str (replicate1 1 0)
                  (\input _ -> (fromZero $ sum input) / i2f n)

movingAvg2 :: (Fraction a, RealFloat a)
           => WordN -> Stream (Data a) -> Stream (Data a)
movingAvg2 n str = recurrenceIO2 (P.replicate (P.fromIntegral n) 0) str []
                   (\input _ -> (P.sum input) / i2f (value n))
