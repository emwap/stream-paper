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
using mutation while retaining the advantages of the
functional stream representation. Our contributions are:

* A new stream representation, `M (M a)`, for some monad `M` which
  supports mutation. The new representation, while relying
  on imperative features, can still be given a functional interface,
  just like the functional stream representation.

* We show how to optimize the representation to eliminate duplicate
  loop variables.

* We show how our new stream representation can be used in EDSLs.
  It is currently used in the Feldspar language.

* We present performance evaluation, showing TODO.

# The Problem

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
streams were we can use mutation to efficiently implement such
functions.

# Efficient Monadic Streams

We present a new representation for streams which uses monads to
enable mutation.

~~~ {.haskell}
data Stream a = Stream (IO (IO a))
~~~

It is straightforward to parameterize this representation on the
particular choice of monad. We use the `IO` monad here for the sake of
concreteness.

Why does the representation have two levels of monads? The key to
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

# Avoiding Multiple Loop Variables

The stream representation already presented allows for mutation
which improves efficiency of the generated code
considerably. The generated code still suffers from a problem where
fused functions will cause multiple loop indices to appear in the same
loop. Consider the following pattern:

~~~ {.haskell}
foo arr = remember n $ .. $ cycle arr
~~~

The generated code for this pattern contains one loop index
originating from `cycle` and one from `remember`:

~~~ {.C}
for (int i = 0; i < ..; i++) {
  TODO: Add correct code;
  ...
}
~~~

Good C compilers might remove multiple loop indices but relying on the
C compiler to perform that optimization on signal processing
applications is a risk. The mere presence of multiple loop indices
might prevent earlier optimizations at the functional level from
kicking in.


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
generate better code:

~~~ {.C}
for (int i = 0; i < ..; i++) {
  TODO: Add example of better code
  ...
}
~~~

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
`(a,s)` and `N a` be `(s -> (a,s))` if we forego mutation. We
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

# Related Work

\paragraph{Lazy streams}
Streams can be represented succinctly in lazy languages like Haskell
with the following definition:

~~~ {.haskell}
data Stream a = Cons a (Stream a)
~~~

Lazy streams suffer from the same problem as the functional stream
representation presented earlier in the paper: some algorithms are not
possible to implement efficiently. The monadic variant of this type
is:

~~~ {.haskell}
type Stream a = M (Stream' a)

data Stream' a = Cons a (Stream a)
~~~

The above definition enables the use of mutation which allows for more
efficient implementations of filters. However, recursive definitions
are problematic in the context of code generating EDSLs. The monadic
formulation of streams we have presented has the advantage of being
usable even in an EDSL context.

TODO Recursive definitions are also problematic for fusion, no?

\paragraph{Stream fusion}

\paragraph{Conduits, Pipes, etc.}
There are many Haskell libraries for dealing with streaming data, such as Conduit [@conduit-overview] and Pipes [@pipes]. Most of these libraries define streams over an underlying monad. Typically, the underlying monad is the `IO` monad, which then allows for the streaming programs to perform external communication. However, there is nothing stopping from using the `IO` monad also for "internal" effects, such as mutable state.

Stream representations such as the one in Conduits can describe more general networks than our `Stream` type (e.g. nodes with different input and output rates). However, being based on recursive definitions, those stream programs are generally not guaranteed to fuse. Though, when certain requirements are met, conduits are subject to fusion [@conduit-fusion].

The fusion framework in Conduits relies on GHC rules to rewrite recursive stream programs to corresponding programs based on a non-recursive stream type:

~~~ {.haskell}
data Stream m o r = forall s . Stream (s -> m (Step s o r)) (m s)
~~~

This type is quite close to our `Stream` representation: the initialization action `m s` can be used to initialize mutable state, and the step function can be used to mutate this state. The main difference is that there is still immutable state passed around (of type `s`), which is unnecessary if we put all the state in the monad.

\paragraph{Machines}

\paragraph{Vector} (has an implementation of finite (monadic) streams)

\paragraph{FRP}

## Future Work

# References
