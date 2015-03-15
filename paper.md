---
documentclass: 'llncs'
title: Streams and stuff
author: Grabbarna Grus
abstract:

  We have solved all the worlds problems

---

# Introduction

A popular functional stream representation is a transition function
from and old state to an element and a new state, together with a
starting state:

~~~ .haskell
data Stream a = forall s . Stream (s -> (a,s)) s
~~~

The representation is expressive and can be compiled into efficient
code by means of fusion.

Computing a simple moving average over a stream is done by keeping
track of the most recent values from the stream with a sliding
window. Computing a new result can be done by inserting a new value at
the front of the window and removing an element from the back. The
typical functional implementation of a sliding window requires the
whole history except the last element to be copied to avoid aliasing
problems. Copying the history is safe and conceptually simple but
performance suffers. Even if performance is sufficient, copying data
consumes more power and generates more heat.

This paper presents a new representation of streams which allows for
using in-place updates while retaining the advantages of the
functional stream representation. Our contributions are:

* A new stream representation, `M (M a)`, for some monad `M` which
  supports in-place updates. The new representation, while relying
  on imperative features, can still be given a functional interface,
  just like the functional stream representation.

* We show how to optimize the representation to eliminate duplicate
  loop variables.

* We show how our new stream representation can be used in EDSLs.
  It is currently used in the Feldspar language.

* We present performance evaluation, showing TODO.

# The problem

Consider implementing an fir filter, a common example of a digital
filter. 


# A new representation for streams

~~~ {.haskell}
data Stream a = Stream (IO (IO a))
~~~

~~~ {.haskell}
map :: (a -> b) -> Stream a -> Stream b
map f (Stream init) = Stream $ do
  next <- init
  loop $ do
    a <- next
    return (f a)
~~~

~~~ {.haskell}
pre :: a -> Stream a -> Stream a
pre v (Stream init) = Stream $ do
    next <- init
    r <- newIORef v
    loop $ do
      a <- next
      b <- readIORef r
      writeIORef r a
      return b
~~~

An infinite stream cannot be stored in memory. Saving a prefix of an
infinite stream for later processing is often convenient. Below is the
code for saving:

TODO: Rename alloc.

~~~ {.haskell}
alloc :: Stream a -> Int -> IO (Array Int a)
alloc (Stream init) len = do
  arr  <- newArray_ (0,len-1)
  next <- init
  forM [0..len-1] $ \i -> do
    a <- next
    writeArray arr i a
  freeze arr
~~~

The code starts by allocating a mutable array of the appropriate
size, followed by an initialization of the array. The initialization
produces the `next` function which is used in the loop body to produce
new elements in the stream which are successively stored in the array.
When the loop is done, the mutable array is frozen, returning a
immutable array as the final result.

We are now in a position to implement an efficient fir filter using
the imperative features of the new monadic stream representation.

~~~ {.haskell}
fir :: Array Int a -> Stream a -> Stream a
fir b inp =
    recurrence (replicate (length b) 0) inp
               (scalarProd b)

recurrence :: Array Int a -> Stream a ->
              (Array Int a -> Array Int b -> b) ->
              Stream b
recurrence ii (Stream init) mkExpr = Stream $ do
    next <- init
    ibuf <- initBuffer ii
    loop $ do
      a <- next
      when (lenI /= 0) $ putBuf ibuf a
      b <- withBuf ibuf $ \ib ->
             return $ mkExpr ib
      return b
  where
    lenI = length ii
~~~

## Relation to Functional Streams

The functional representation of streams can be recovered from our
monadic representation to shed new light on the monadic represenation.
Consider again the type `M (M a)`. The outer and the inner monads are
the same. But they don't have to be. We could imagine a representation
`M (N a)`. The monad `M` would be responsible for initializing memory
while the monad `N` would be reading and writing that memory.  If we
forego mutation, we can let `M a` be `(a,s)` and `N a` be `(s ->
(a,s))`. We recognize them as the writer monad and the state
monad. These two monads, when combined, results in the functional
stream representation.

## Streams for EDSLs

Our new monadic representation of streams is a natural fit for
embedded domain specific languages and work particularly well with the
technique of combining shallow and deep embeddings
[@svenningsson2013combining]. Monads can be embedded in an EDSL using
the technique from [@genericmonads11].  Embedding monads in this way
has the advantage of applying the right monads laws need for fusion
for free.

The language Feldspar [@FeldsparIFL2010] has a stream library using a
monadic embedding. The stream library is almost identical to the
Haskell library presented in section \ref{}. In particular, the type
of streams can be the same, except for using a different, embedded
monad. The only difference for the programmer is that Feldspar
requires some constraints on functions which allocate to memory, since
not all Haskell types can be allocated in Feldspar.

# Avoiding multiple loop variables

~~~
data Stream a = Stream (IO (Int -> IO a))
~~~

# Evaluation

# Related work

## FRP

## Conduits and pipes

## Machines

## Future Work

