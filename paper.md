---
documentclass: 'llncs'
title: Streams and stuff
author:
 - Josef Svenningsson\inst{1}
 - Emil Axelsson\inst{1}
 - Anders Persson\inst{1}
 - Peter A. Jonsson\inst{2}
institute:
 - Chalmers University of Technology
 - SICS Swedish ICT AB
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

~~~ {.haskell}
data Stream a = forall s . Stream (s -> (a,s)) s
~~~

The representation is expressive and can be compiled into efficient
code by means of fusion.

Consider computing a simple moving average over a stream using the
above representation. The implementation would keep track of the most
recent values from the stream with a sliding window. Computing a new
result involves inserting a new value at the front of the window
and removing an element from the back. The typical functional
implementation of a sliding window requires the whole history except
the last element to be copied to avoid aliasing problems. Copying the
history is safe and conceptually simple but performance suffers. Even
if performance is sufficient, copying data consumes more power and
generates more heat.

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

Consider again the functional stream representation from the
introduction.

~~~ {.haskell}
data Stream a = forall s. Stream (s -> (a,s)) s
~~~

Implementing an algorithm such as the moving average using the above
representation can look as follows:

~~~ {.haskell}
movingAvg n (Stream step init) = Stream step' init'
  where
    init' = (init, listArray (0,n-1) replicate n 0)
    step' (s,window) =
      let (s',a)  = step s
          window' = ixmap (0,n) (\i -> i+1 `mod` n) window 
                 // [(n-1,a)]
      in (avg window, (s',window'))
    avg w = sum (elems w) / n
~~~

This implementation is inefficient because the window needs to be
copied each iteration, even if the operations `ixmap` and `\\` are
fused. Copying can be avoided to some extent by using smarter window
representations. The smart representations tend to have a high
constant overhead making them unsuitable for the common case of small
window sizes.

Similar problems appear for many applications of streams, such as
digital fir and iir filters. What we would like is a representation of
streams were we can use in-place updates to efficiently implement such
functions.

# Efficient Monadic Streams

We present a new representation for streams which uses monads to
enable in-place updates.

~~~ {.haskell}
data Stream a = Stream (IO (IO a))
~~~

It is straightforward to parameterize this representation on the
particular choice of monad. We use the `IO` monad here for the sake of
concreteness.

Why does the representation have two levels of monads? We can convert
something of type `IO (IO a)` to `IO a` by using `join`. The key to
understanding this representation is that the outer monadic
computation performs initialization and is only meant to be called
once. The outer monad computes a new monadic computation corresponding
to the inner monad, which is called once for each element in the
stream.

Our new monadic representation of streams can still be given an API
which is functional in flavour and similar to what a programmer would
expect from a functional representation. As an initial example
consider the `map` function:

~~~ {.haskell}
map :: (a -> b) -> Stream a -> Stream b
map f (Stream init) = Stream $ do
  next <- init
  loop $ do
    a <- next
    return (f a)
~~~

The new stream is initialized by running the initialization
computation from the input stream, yielding the step function `next`.
Then, in the new step function, the function `next` is run to produce
an element `a` which transformed by the function `f` and then
returned. The combinator `loop` is defined as `return`. We use the
name `loop` to convey that the code returned by `loop` is executed an
indefinite number of times.

There are various ways of creating streams, below we show the function
`cycle` which cycles through the elements of an array. A particular
aspect of this function is that it has to create a reference which is
used to keep track of what element in the array to read from.

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

The code starts by allocating a mutable array of the appropriate
size, followed by an initialization of the array. The initialization
produces the `next` function which is used in the loop body to produce
new elements in the stream which are successively stored in the array.
When the loop is done, the mutable array is frozen, returning a
immutable array as the final result.

We are now in a position to write  an efficient moving average using
the imperative features of the new monadic stream representation.

~~~ {.haskell}
movingAvg :: Int -> Stream Double -> Stream Double
movingAvg n str = recurrence (listArray (0,n-1) (replicate n 0.0)) str
                   (\input -> sum (elems input) / n)

recurrence :: Array Int a -> Stream a ->
              (Array Int a -> b) ->
              Stream b
recurrence ii (Stream init) mkExpr = Stream $ do
    next <- init
    ibuf <- initBuffer ii
    loop $ do
      a <- next
      putBuf ibuf a
      b <- withBuf ibuf $ \ib ->
             return $ mkExpr ib
      return b
~~~

The core functionality is exposed by the `recurrence` function which
uses a mutable cyclic buffer. The type signatures for the
operations we rely on are:

~~~ {.haskell}
initBuffer :: Array Int a -> IO (Buffer a)
putBuf     :: Buffer a -> a -> IO ()
withBuf    :: Buffer a -> (Array Int a -> b) -> IO b
~~~

The function `initBuffer` creates a new buffer, `putBuf` adds a new
element while discarding the oldest element. The programmer can get an
immutable view of the current contents of the buffer in a local scope
by using `withBuf`. The function `withBuf` can be implemented without
copying but it requires that the programmer does not provide a
function which returns the whole array.

Returning to the function `recurrence`; the input stream stream is
initialized as is the cyclic buffer. For each element in the output
stream an element from the input stream is computed and stored in the
cyclic buffer. The content of the cyclic buffer is processed by
a function provided by the caller of `recurrence` and the result is
returned as the next element in the output stream. The resulting
stream will contain values computed from a sliding window of the
input stream.

The function `movingAvg` uses `recurrence` to provide sliding windows
of the input stream and passes a function to compute the average of
a window. The initial window only contains zeros.

More advanced digital filters, like a fir filters, can be
implemented in a similar fashion to the moving average:

~~~ {.haskell}
fir :: Array Int a -> Stream a -> Stream a
fir b inp =
    recurrence (listArray (0,l-1) (replicate l 0)) inp
               (scalarProd b)
  where l = rangeSize (bounds b)
~~~

Implementing iir filters requires a somewhat more sophisticated
version of `recurrence` which also has a cyclic buffer for the
elements of the output stream.

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

Parameterising the stream representation with the loop
counter solves the problem:

~~~ {.haskell}
data Stream a = Stream (IO (Int -> IO a))
~~~

The function performing allocation is responsible for providing the
loop index. Here is the new version of `remember` for the new
representation:

~~~ {.haskell}
remember :: Int -> Stream a -> IO (Array Int a)
remember len (Stream init) = do
  arr  <- newArray_ (0,len-1)
  next <- init
  forM [0..len-1] $ \i -> do
    a <- next i
    writeArray arr i a
  freeze arr
~~~

The key difference from the previous version is that the loop variable
`i` is fed to the step function `next`.

Functions like `cycle` can now take advantage of the provided loop
index, and don't need to create their own loop variables:

~~~ {.haskell}
cycle :: Array Int a -> Stream a
cycle arr = Stream $ do
  let l = length arr
  loop $ \i -> do
    return (arr!(i `mod` l))
~~~

The new code for `cycle` is considerably shorter and will also
generate better code.

TODO: Add example of better code.

# Streams for EDSLs

Our new monadic representation of streams is a natural fit for
embedded domain specific languages and work particularly well with the
technique of combining shallow and deep embeddings
[@svenningsson2013combining]. Monads can be embedded in an EDSL using
the technique by @genericmonads11. Embedding monads in this way
gives fusion for free since the host language applies the right monads
laws automatically.

Feldspar [@FeldsparIFL2010] has a stream library using a monadic
embedding. The stream library is almost identical to the Haskell
library presented in Section [Efficient Monadic Streams]. The stream
type is the same except using a different embedded monad. The only
difference for the programmer is that Feldspar requires some
constraints on functions which allocate to memory, since not all
Haskell types can be allocated in Feldspar.

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
`(a,s)` and `N a` be `(s -> (a,s))` if we forego in-place updates. We
recognize them as the writer monad and the state monad. Combining
these two monads results in the functional stream representation.

# Finite Streams

Finite streams can also be represented by adding an extra length
parameter to our monadic representation:

~~~ {.haskell}
data Stream a = Stream (IO (IO a)) Int
~~~

Most function definitions for finite stream are similar to those for
infinite streams, with the addition of passing around the length
parameter. Additionally, functions like appending two streams now make
sense, and it is possible to allocate the whole stream to memory.

# Related work

* `data Stream a = Cons a (Stream a)`

* FRP

* Conduits and pipes

* Machines

* Stream fusion

## Future Work

# References
