---
documentclass: 'llncs'
title: Streams and stuff
author:
 - Josef Svenningsson\inst{1}
 - Emil Axelsson\inst{1}
 - Anders Persson\inst{1}
 - Peter Jonsson\inst{2}
institute:
 - Chalmers University of Technology
 - Swedish Institute of Computer Science
abstract:

  1. state the problem
  2. say why it is interesting
     Our representation allows code generation using in-place updates without spurious loop-indices.
     The generated code is XXX% more efficient than the regular functional stream representation.

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


~~~ {.haskell}
cycle :: Array Int a -> Stream a
cycle arr = Stream $ do
  r <- newIORef 0
  let l = length arr
  loop $ do
    i <- readIORef r
    arr!(i `mod` l)
    writeIORef r (i+1)
~~~

An infinite stream cannot be stored in memory. Saving a prefix of an
infinite stream for later processing is often convenient. Below is the
code for saving:

~~~ {.haskell}
remember :: Int -> Stream a -> IO (Array Int a)
remember len (Stream init) = do
  arr  <- newArray_ (0,len-1)
  next <- init
  forM [0..len-1] $ \i -> do
    a <- next
    writeArray arr i a
  freeze arr
~~~
a
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

# Avoiding multiple loop variables

The stream representation already presented allows for in-place
updates which improves efficiency of the generated code
considerably. The generated code still suffers from a problem where
fused functions will cause multiple loop indices to appear in the same
loop. Consider the following function:

~~~ {.haskell}
foo arr = remember n $ map (+1) $ cycle arr
~~~

The generated code for this function will contain two loop indices,
one originating from `cycle` and one from `remember`. Good C compilers
might remove multiple loop indices but relying on the C compiler to
perform that optimization on signal processing applications is a
risk. The mere presence of multiple loop indices might prevent earlier
optimizations at the functional level from kicking in.

Parameterising the stream representation with respect to the loop
counter solves the problem:

~~~ {.haskell}
data Stream a = Stream (IO (Int -> IO a))
~~~

# Streams for EDSLs

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

# Evaluation

# Relation to Functional Streams

The functional representation of streams can be recovered from our
monadic representation to shed new light on the monadic
representation.  Consider again the type `M (M a)`. The outer and the
inner monads are the same but they could be different as long as the
necessary functionality is provided by the respective monads. We could
imagine a representation `M (N a)` where the outer monad `M` is
responsible for initializing memory and the inner monad `N` is
responsible for reading and writing that memory.  We can let `M a` be
`(a,s)` and `N a` be `(s -> (a,s))` if we forego mutation. We
recognize them as the writer monad and the state monad. These two
monads, when combined, results in the functional stream
representation.

# Related work

## FRP

## Conduits and pipes

## Machines

## Future Work

