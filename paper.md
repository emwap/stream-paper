---
documentclass: 'llncs'
title: Efficient Monadic Streams
author:
 - Josef Svenningsson\inst{1}
 - Emil Axelsson\inst{1}
 - Anders Persson\inst{1}
 - Peter A. Jonsson\inst{2}
institute:
 - Chalmers University of Technology
 - SICS Swedish ICT AB
abstract:
  Functional stream representations allow for a high level,
  compositional way of programming digital signal processing
  algorithms. However, some algorithms, such as filters, cannot
  be efficiently implemented using purely functional techniques,
  due to excessive copying of data.

  We present a monadic representation of stream which introduces the
  ability to use mutation for efficiency when implementing
  algorithms. Still, our representation enjoys many of the benefits of
  purely functional streams, such as a functional API and fusion.
  Our representation enables further optimizations: we show how to
  remove duplicate loop variables.

  Our measurements show that our new monadic representation
  consistently outperforms the functional representation by more than
  an order of magnitude.
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

* We demonstrate a performance
  advantage of XXX\% compared to YYY when using our monadic representation.

We have used our new monadic formulation of stream in Feldspar and
will evaluate the improvement in the context of Feldspar. However, we
will for the most part use Haskell code to demonstrate the technique,
in order to make the presentation more accessible.

# The Problem

Consider the functional stream representation from the
introduction again.

~~~ {.haskell}
data Stream a = forall s. Stream (s -> (a,s)) s
~~~

Implementing an algorithm such as the moving average using the above
representation can look as follows:

~~~ {.haskell}
movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n (Stream step init) = Stream step' init'
  where
    init' = (init, listArray (0,n-1) (replicate n 0))
    step' (s,window) =
      let (a,s')  = step s
          window' = ixmap (0,n) (\i -> i+1 `mod` n) window
                 // [(n-1,a)]
      in (avg window, (s',window'))
    avg w = sum (elems w) / fromIntegral n
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
once. The outer monadic computation returns a new monadic
computation of type `M a`. This inner computation is called once for
each element in the stream.

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
an element `a` which is transformed by the function `f` and then
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
    writeIORef r (i+1)
    return (arr!(i `mod` l))
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
When the loop is done, the mutable array is frozen, returning an
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
             mkExpr ib
      return b
~~~

The core functionality is exposed by the `recurrence` function which
uses a mutable cyclic buffer. The type signatures for the
operations we use are:

~~~ {.haskell}
initBuffer :: Array Int a -> IO (Buffer a)
putBuf     :: Buffer a -> a -> IO ()
withBuf    :: Buffer a -> (Array Int a -> b) -> IO b
~~~

The function `initBuffer` creates a new buffer, `putBuf` adds a new
element while discarding the oldest element. The programmer can get an
immutable view of the current contents of the buffer in a local scope
by using `withBuf`. The function `withBuf` can be implemented without
copying but program correctness relies on the programmer to ensure
that the provided function does not return the whole array.

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

More advanced digital filters, like fir filters, can be
implemented in a similar fashion to the moving average:

~~~ {.haskell}
fir :: Num a => Array Int a -> Stream a -> Stream a
fir b inp = recurrence (listArray (0,l-1) (replicate l 0)) inp
                       (scalarProd b)
  where l = rangeSize (bounds b)
~~~

Implementing iir filters requires a somewhat more sophisticated
version of `recurrence` which also has a cyclic buffer for the
elements of the output stream.

# Fusion

The functional stream representation supports fusion, meaning that
intermediate streams are removed. Stream fusion [@coutts2007stream] is
a good demonstration of this although it employs a slightly more
sophisticated stream representation.

Our new monadic representation also supports fusion in a similar
fashion to the functional representation. A key difference is that two
of the monad laws are essential to achieve good code generation: the
left identity law and associativity of bind. Here is the left identity
law:

~~~{.haskell}
do a <- return x
   f a
==
do f x
~~~

And the following demonstrates the associativity of bind:

~~~{.haskell}
do b <- do a <- m
           f a
   g b
==
do a <- m
   b <- f a
   g b
~~~

We demonstrate fusion using a concrete example: `map f . map g`. To
refresh our memories we repeat the definition of `map` below:

~~~ {.haskell}
map :: (a -> b) -> Stream a -> Stream b
map f (Stream init) = Stream $ do
  next <- init
  loop $ do
    a <- next
    return (f a)
~~~

What follows is a derivation of an efficient implementation of
`map f . map g`. Each step is annotated with the law used in the
transformation. In order to get fusion going we will apply
`map f . map g` to concrete but arbitrary stream `Stream init`.

~~~ {.haskell}
map f (map g (Stream init))

=> { inlining map }

map f (Stream $ do
  next <- init
  loop $ do
    a <- next
    return (f a)

=> { inlining map }

Stream $ do
  next' <- do
    next <- init
    loop $ do
    a <- next
    return (g a)
  loop $ do
    b <- next'
    return (f b)

=> { bind associativity }

Stream $ do
  next <- init
  next' <- loop $ do
    a <- next
    return (g a)
  loop $ do
    b <- next'
    return (f b)

=> { loop = return, left identity }

Stream $ do
  next <- init
  loop $ do
    b <- do
      a <- next
      return (g a)
    return (f b)

=> { bind associativity }

Stream $ do
  next <- init
  loop $ do
    a <- next
    b <- return (g a)
    return (f b)

=> { left identity }

Stream $ do
  next <- init
  loop $ do
    a <- next
    return (f (g a))
~~~

The final result is as efficient as one can possible hope for.

Fusing combinators other than `map` follows a similar pattern.

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
originating from `cycle` and one from `remember`. Good C compilers might remove multiple loop indices but relying on the
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
generate better code.

# Streams for EDSLs

Our new monadic representation of streams is a natural fit for
embedded domain specific languages and works particularly well with
the technique of combining shallow and deep embeddings
[@svenningsson2013combining]. Monads can be embedded in an EDSL using
the technique by @genericmonads11. Embedding monads in this way is
particularly attractive: the embedding of monads applies the two monad
laws by evaluation in the host language. This means that the kind of
rewriting explained in the [Fusion] section happens automatically, no
extra code needs to be written in order to achieve the optimization.

Feldspar [@FeldsparIFL2010] has a stream library that uses a monadic
embedding. The stream library is almost identical to the Haskell
library presented in Section [Efficient Monadic Streams]. The stream
type is the same except using a different embedded monad. The only
difference for the programmer is that Feldspar requires some
constraints on functions which allocate memory, since not all
Haskell types can be allocated in Feldspar.

# Evaluation

\begin{figure}[tp]
\begin{tikzpicture}
 \begin{axis}[
      height=0.6\textwidth,
      width=0.8\textwidth,
      title=FIR filter,
      title style={at={(0.5,0.94)},anchor=south},
      xlabel=\scriptsize{filter order},
      ylabel=s,
      every axis x label/.style={at={(1,-0.09)},anchor=north east},
      every axis y label/.style={at={(-0.1,0.65)},anchor=east},
      legend entries={\scriptsize{C reference},\scriptsize{Pure},\scriptsize{Monadic}},
      legend style={at={(0.03,0.93)},anchor=north west},
      cycle list={blue,mark=*\\%
                  red,mark=square*\\%
                  brown,mark=+\\%
                 }
    ]
    \addplot shell[prefix=pgfshell_,id=ref]
        { awk -F'/|,' '/c_fir_ref/ { print $2,$5 }' benchmark/benchmark.csv};
    \addplot shell[prefix=pgfshell_,id=pure]
        { awk -F'/|,' '/c_fir_old/ { print $2,$5 }' benchmark/benchmark.csv};
    \addplot shell[prefix=pgfshell_,id=monadic]
        { awk -F'/|,' '/c_fir2_bench/ { print $2,$5 }' benchmark/benchmark.csv};
 \end{axis}
\end{tikzpicture}
\caption{Running time of filters compared to reference C implementations.}
\label{fig:measurements-fir}
\end{figure}

\begin{figure}[tp]
\begin{tikzpicture}
 \begin{axis}[
      height=0.6\textwidth,
      width=0.8\textwidth,
      title=Moving Average filter,
      title style={at={(0.5,0.94)},anchor=south},
      xlabel=\scriptsize{filter order},
      ylabel=s,
      every axis x label/.style={at={(1,-0.09)},anchor=north east},
      every axis y label/.style={at={(-0.1,0.65)},anchor=east},
      legend entries={\scriptsize{Pure},\scriptsize{Monadic}},
      legend style={at={(0.03,0.93)},anchor=north west},
      cycle list={blue,mark=*\\%
                  red,mark=square*\\%
                  brown,mark=+\\%
                 }
    ]
%    \addplot shell[prefix=pgfshell_,id=ref]
%        { awk -F'/|,' '/c_fir_ref/ { print $2,$5 }' benchmark/benchmark.csv};
    \addplot shell[prefix=pgfshell_,id=pure]
        { awk -F'/|,' '/c_mov_avg_old/ { print $2,$5 }' benchmark/benchmark.csv};
    \addplot shell[prefix=pgfshell_,id=monadic]
        { awk -F'/|,' '/c_mov_avg_bench/ { print $2,$5 }' benchmark/benchmark.csv};
 \end{axis}
\end{tikzpicture}
\caption{Running time of filters compared to reference C implementations.}
\label{fig:measurements-mov-avg}
\end{figure}

We have measured the difference between functional and monadic streams
in Feldspar across three difference benchmarks: moving average, fir-
and iir filters. Apart from the Feldspar version, we also have a
handwritten C benchmark to get a baseline for our measurements.
However, it is not entirely an apples-to-apples comparison since
the monadic stream implementation keeps the buffer in registers and
unrolls the loop for updating the buffer. Yet, the measurements give
some indication of how performant our implementation is.

# Relation to Functional Streams

The functional representation of streams can be recovered from the
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

# A Pure Interface

We have used `IO` as the monad for mutability so far. We can get away with using the `ST` monad  f we only care about mutability and not about general effects that affect the external world:

~~~ {.haskell}
data Stream s a = Stream (ST s (ST s a))
~~~

We can reimplement all stream functions for this new representation by using `STRef` instead of `IORef` and `STArray` instead of `IOArray`. The advantage of using `ST` becomes visible in functions that consume streams, such as `remember`:[^remember]

~~~ {.haskell}
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
~~~

[^remember]: The helper function `remember'` is needed to get the types right.

The result is now a pure `Array` value rather than a monadic one. This means that we can hide all uses of monads from the user and provide a pure interface to streams. However, in order to make use of mutability, one has still to write monadic code (or use canned solutions, such as `recurrence`).

# Related Work

\paragraph{\bf Lazy streams}
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
are problematic in the context of code generating EDSLs. Our monadic
representation of streams has the advantage of being
usable even in an EDSL context.

\paragraph{\bf Coiterative streams}
A coiterative representation of streams makes it possible to avoid recursion in the definition of streams and in functions defined for streams [@Caspi19981]. Our initial representation in section \ref{the-problem} was based on coiteration.

The stream fusion framework [@coutts2007stream] builds on the following coiterative representation:

~~~ {.haskell}
data Stream a = forall s . Stream (s -> Step a s) s
~~~

The difference to our initial representation is the `Step` type that is returned by the step function. The `Step` type has three cases: (1) a pair of an element `a` and a new state `s`, or (2) just a new state, or (3) a value signaling that the stream has ended. This stream representation makes it possible to give efficient definitions of many stream operations and make sure that streams are fused when operations are composed. The stream fusion framework uses GHC rewrite rules to convert list-based code to stream-based code where possible.

In contrast to our work, stream fusion does not support streams with mutable state.

<!--
TODO We could mention the fact that the Vector package has a co-iterative stream representation with an extra length argument, like our finite streams. But this is not a big thing in our paper, so I'll skip it for now.

A suitable reference could be "Exploiting vector instructions with generalized stream fusion" (Mainland et al., ICFP 2013) http://dl.acm.org/citation.cfm?id=2500601
-->

\paragraph{\bf Effectful stream programming}
There are many Haskell libraries for dealing with streaming data, such as Conduit [@conduit-overview], Pipes [@pipes] and Iteratees [@kiselyov2012iteratees]. Most of these libraries define streams over an underlying monad. Choosing `IO` as the underlying monad allows for the streaming programs to perform external communication. However, there is nothing stopping from using the `IO` monad also for "internal" effects, such as mutable state.

Stream representations such as the one in Conduits can describe more general networks than our `Stream` type (e.g. nodes with different input and output rates). However, being based on recursive definitions, those stream programs are generally not guaranteed to fuse. Though, when certain requirements are met, conduits are subject to fusion [@conduit-fusion].

The fusion framework in Conduits relies on GHC rules to rewrite recursive stream programs to corresponding programs based on a non-recursive stream type (an extension of the stream fusion representation above):

~~~ {.haskell}
data Stream m o r = forall s . Stream (s -> m (Step s o r)) (m s)
~~~

This type is quite close to our `Stream` representation: the initialization action `m s` can be used to initialize mutable state, and the step function can be used to mutate this state. The main difference is that there is still immutable state of type `s` passed around, which is unnecessary if we put all the state in the monad.

\paragraph{\bf FRP}

Functional Reactive Programming (FRP) was initially concieved as a way
to program compositionally with time varying values where time is
treated continuously [@elliott1997functional]. In contrast, many
implementations of FRP use a discrete notion of time
[@nilsson2002functional; @czaplicki2013asynchronous; @patai2011efficient]. Discrete
time FRP and streams are similar in many respects, as explained by
@wan2000functional. However, while the goal of FRP is often on
expressivity, we use streams in the context of digital signal
processing where we are happy to trade expressivity for efficiency.
Perhaps some of the techniques presented in this paper can be applied
to speed up FRP implementations; such investigations are future work.

# References
