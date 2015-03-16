module Buffer where



import Data.Array.IO
import Data.IORef



-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Int -> IO a
    , putBuf   :: a -> IO ()
    }

arrLength arr = do
    (l,h) <- getBounds arr
    return (h-l)

--- | Create a new cyclic buffer
initBuffer' :: IOArray Int a -> IO (Buffer a)
initBuffer' buf = do
    l  <- arrLength buf
    ir <- newIORef 0
    let get j = do
          i <- readIORef ir
          readArray buf $ calcIndex l i j
        put a = do
          i <- readIORef ir
          writeIORef ir ((i+1) `mod` l)
          writeArray buf i a
    return (Buffer get put)
  where
    calcIndex l i j = (l+i-j-1) `mod` l

-- | Create a new cyclic buffer initalized by the given vector (which also determines the size)
initBuffer :: [a] -> IO (Buffer a)
initBuffer buf = newListArray (0, length buf-1) buf >>= initBuffer'

-- | Create a new cyclic buffer of the given length initialized by the given element
newBuffer :: Int -> a -> IO (Buffer a)
newBuffer l init = initBuffer (replicate l init)

