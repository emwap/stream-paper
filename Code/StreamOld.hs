{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module StreamOld where

import qualified Prelude
import Feldspar
import Feldspar.Vector
import Feldspar.Mutable
import Feldspar.Compiler

data Stream a = forall s . Syntax s => Stream (s -> (a,s)) s

singleton :: Data a -> Pull1 a
singleton a = indexed1 1 (const a)

recurrenceI :: (Type a, Type b)
            => Pull1 a -> Stream (Data a) ->
               (Pull1 a -> Data b) ->
               Stream (Data b)
recurrenceI ii (Stream step init) mkExpr = Stream step' init'
  where init' = (init,store ii)
        step' (s,ii) = let (a,s') = step s
                           ii' = store $ singleton a ++ Feldspar.Vector.take (lenI - 1) ii
                           b = mkExpr (toPull ii')
                           lenI = length ii
                       in (b,(s',ii'))


recurrenceIO :: (Type a, Type b)
             => Pull1 a -> Stream (Data a) -> Pull1 b ->
                (Pull1 a -> Pull1 b -> Data b) ->
                Stream (Data b)
recurrenceIO ii (Stream step init) io mkExpr = Stream step' init'
  where init' = (init,store ii,store io)
        step' (s,ii,io) = let (a,s') = step s
                              ii' = store $ singleton a ++ Feldspar.Vector.take (lenI - 1) ii
                              io' = store $ singleton b ++ Feldspar.Vector.take (lenO - 1) io
                              b = mkExpr (toPull ii') (toPull io)
                              lenI = length ii
                              lenO = length io
                          in (b,(s',ii',io'))

take :: (Type a) => Data Length -> Stream (Data a) -> Data [a]
take n (Stream step init) = runMutableArray $ do
  arr <- newArr_ n
  r <- newRef init
  forM n $ \ix -> do
    s <- getRef r
    let (a,s') = step s
    setArr arr ix a
    setRef r s'
  return arr

streamAsVector :: (Type b)
               => (Stream (Data a) -> Stream (Data b)) ->
                  (Pull1 a -> Pull1 b)
streamAsVector f vec = thawPull1 $ StreamOld.take (length vec) $
                       f (Stream (\i -> (vec!:(Z:.i),i+1)) 0)

movingAvg :: (Fraction a, RealFloat a)
          => Data WordN -> Stream (Data a) -> Stream (Data a)
movingAvg n str = recurrenceIO (replicate1 n 0) str (replicate1 1 0)
                  (\input _ -> (fromZero $ sum input) / i2f n)

-- streamAsVector f vec = toPull $ arrToManifest (fromList [length vec], StreamOld.take (length vec) $
--                        f (Stream (\i -> (vec!:(Z:.i),i+1)) 0))

-- | A fir filter on streams
fir :: Numeric a => Pull1 a ->
       Stream (Data a) -> Stream (Data a)
fir b inp =
    recurrenceIO (replicate1 (length b) 0) inp (replicate1 1 0)
                 (\i _ -> scalarProd b i)
  -- Temporarily using recurrenceIO instead of recurrenceI, because the latter uses an empty output
  -- buffer, which triggers https://github.com/Feldspar/feldspar-language/issues/24

-- | An iir filter on streams
iir :: Fraction a => Data a -> Pull1 a -> Pull1 a ->
       Stream (Data a) -> Stream (Data a)
iir a0 a b inp =
    recurrenceIO (replicate1 (length b) 0) inp
                 (replicate1 (length a) 0)
      (\i o -> 1 / a0 * ( scalarProd b i
                        - scalarProd a o)
      )

