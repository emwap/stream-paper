{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module StreamOld where

import qualified Prelude
import Feldspar
import Feldspar.Vector
import Feldspar.Compiler

data Stream a = forall s . Syntax s => Stream (s -> (a,s)) s

(+++) :: (Type a) => Pull1 a -> Pull1 a -> Pull1 a
v1 +++ v2 = indexed1 (length v1 + length v2) $ \i ->
              condition (i < length v1) (v1!:(Z:.i)) (v2!:(Z:.(i-length v1)))

recurrenceI :: (Type a, Type b)
            => Pull1 a -> Stream (Data a) ->
               (Pull1 a -> Data b) ->
               Stream (Data b)
recurrenceI ii (Stream step init) mkExpr = Stream step' init'
  where init' = (init,ii)
        step' (s,ii) = let (a,s') = step s
                           ii' = Feldspar.Vector.take (lenI - 1) ii +++ indexed1 1 (const a)
                           b = mkExpr ii'
                           lenI = length ii
                       in (b,(s',ii'))


recurrenceIO :: (Type a, Type b)
             => Pull1 a -> Stream (Data a) -> Pull1 b ->
                (Pull1 a -> Pull1 b -> Data b) ->
                Stream (Data b)
recurrenceIO ii (Stream step init) io mkExpr = Stream step' init'
  where init' = (init,ii,io)
        step' (s,ii,io) = let (a,s') = step s
                              ii' = Feldspar.Vector.take (lenI - 1) ii +++ indexed1 1 (const a)
                              io' = Feldspar.Vector.take (lenO - 1) io +++ indexed1 1 (const b)
                              b = mkExpr ii' io
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

