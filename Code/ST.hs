module ST where



import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.Unsafe (unsafeFreeze)



data Stream s a = Stream (ST s (ST s a))

remember :: Int -> (forall s . Stream s a) -> Array Int a
remember len str = runSTArray $ remember' len str

remember' :: Int -> Stream s a -> ST s (STArray s Int a)
remember' len (Stream init) = do
  arr  <- newArray_ (0,len-1)
  next <- init
  forM [0..len-1] $ \i -> do
    a <- next
    writeArray arr i a
  return arr



data Buffer s a = Buffer
    { putBuf  :: a -> ST s ()
    , withBuf :: forall b . (Array Int a -> b) -> ST s b
    }

initBuffer :: forall a s . Array Int a -> ST s (Buffer s a)
initBuffer init = do
    arr <- thaw init
    let put  a = writeArray (arr :: STArray s Int a) 0 a
        with :: (Array Int a -> b) -> ST s b
        with k = do
          arr' <- unsafeFreeze arr
          return $ k (arr' :: Array Int a)
    return $ Buffer put with

-- The type of `withBuf` forces us to use `unsafeFreeze` here. I think the only way to make it safe
-- would be to give `withBuf` the type
--
--   (STArray s Int a -> ST s b) -> ST s b
--
-- which would make `recurrence` less nice.

loop :: Monad m => a -> m a
loop = return

recurrence :: Array Int a -> Stream s a ->
              (Array Int a -> b) ->
              Stream s b
recurrence ii (Stream init) mkExpr = Stream $ do
    next <- init
    ibuf <- initBuffer ii
    loop $ do
      a <- next
      putBuf ibuf a
      b <- withBuf ibuf $ \ib -> mkExpr ib
      return b

