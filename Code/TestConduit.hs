module TestConduit where



import Control.Monad.IO.Class

import Data.Conduit
import qualified Data.Conduit.List as CL

import Buffer
import qualified Reference



dotProd :: Num a => [a] -> Buffer a -> IO a
dotProd as buf = go (reverse as) 0 0
  where
    go []     _ s = return s
    go (a:as) i s = do
        b <- indexBuf buf i
        go as (i-1) (s + a*b)

fir :: Num a => [a] -> Conduit a IO a
fir bs = do
    buf <- liftIO $ initBuffer bs
    loop buf
  where
    loop buf = do
      ma <- await
      case ma of
        Nothing -> return ()
        Just a  -> do
          s <- liftIO $ putBuf buf a >> dotProd bs buf
          yield s
          loop buf

testFir = CL.sourceList (Reference.inp 1000) $$ fir (Reference.bCoeffs 7) =$ CL.fold (+) 0

