module StreamOld where

import Feldspar
import Feldspar.Vector

data Stream a = forall s . Syntax s => Stream (s -> (a,s)) s

recurrenceI :: Pull1 a -> Stream (Data a) ->
               (Pull1 a -> Data b) ->
               Stream (Data b)
recurrenceI ii (Stream step init) mkExpr = Stream step' init'
  where init' = (init,store ii)
        step' (s,ii) = let (a,s') = step s
                           ii' = take (lenI - 1) ii ++ indexed1 1 (const a)
                           b = mkExpr ii'
                       in (b,(s',store ii'))


recurrenceIO :: Pull1 a -> Stream (Data a) -> Pull1 b ->
                (Pull1 a -> Pull1 b -> Data b) ->
                Stream (Data b)
recurrenceIO ii (Stream step init) io mkExpr = Stream step' init'
  where init' = (init,store ii,store io)
        step' (s,ii,io) = let (a,s') = step s
                              ii' = take (lenI - 1) ii ++ indexed1 1 (c
                              b = mkExpr ii' io
onst a)
                              io' = take (lenO - 1) io ++ indexed1 1 (const b)
                          in (b,(s',store ii',store io'))

take :: Data Length -> Stream (Data a) -> Data [a]
take n (Stream step init) = runMutableArray $ do
  arr <- newArray_ n
  r <- newRef init
  forM n $ \ix -> do
    s <- getRef r
    let (a,s') = step s
    setArr arr ix a
    setRef r s'
  return arr

streamAsVector :: (Stream (Data a) -> Stream (Data b)) ->
                  (Pull1 a -> Pull1 b)
streamAsVector f vec = toPull $ take (length vec) $
                       f (Stream (\i -> (vec!i,i+1)) 0)
