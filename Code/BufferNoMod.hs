{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BufferNoMod where

import qualified Prelude
import Control.Applicative

import Feldspar
import Feldspar.Vector
import Feldspar.Mutable hiding (Buffer(..),initBuffer')

-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Data Index -> M a
    , putBuf   :: a -> M ()
    , withBuf  :: forall b . Syntax b => (Data Index -> Pull DIM1 a -> M b) -> M b
    }

-- Another option would be to represent a buffer as its state (the counter and the array), but the
-- above representation leaves room for other implementations.

--- | Create a new cyclic buffer
initBuffer' :: forall a . Syntax a => Data (MArr (Internal a)) -> M (Buffer a)
initBuffer' buf = do
    l  <- arrLength buf
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf $ calcIndex l i j
        put a = do
          i <- getRef ir
          setArr buf i (desugar a)
          modifyRef ir (\x -> condition (x+1>=l) 0 (x+1))
        with :: (Syntax a, Syntax b) => (Data Index -> Pull DIM1 a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f i . freeze)
    return (Buffer get put with)
  where
    calcIndex l i j = (l+i-j-1) `mod` l

    freeze :: Syntax a => Data [Internal a] -> Pull DIM1 a
    freeze = map sugar . thawPull1

-- | Create a new cyclic buffer initalized by the given vector (which also determines the size)
initBuffer :: Syntax a => Pull DIM1 a -> M (Buffer a)
initBuffer buf = thawArray (freezePull1 $ map desugar buf) >>= initBuffer'

-- | Create a new cyclic buffer of the given length initialized by the given element
newBuffer :: Syntax a => Data Length -> a -> M (Buffer a)
newBuffer l init = newArr l (desugar init) >>= initBuffer'

-- | Create a new cyclic buffer of the given length without initialization
newBuffer_ :: Syntax a => Data Length -> M (Buffer a)
newBuffer_ l = newArr_ l >>= initBuffer'
