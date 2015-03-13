---
documentclass: 'llncs'
title: Streams and stuff
author: Grabbarna Grus
abstract:

  We have solved all the worlds problems

---

# Introduction

A popular way of representing streams is as a transition function
from and old state to an element and a new state, together with a
starting state:

~~~ .haskell
data Stream a = forall s . Stream (s -> (a,s)) s
~~~

The functional representation above has the advantage of being very
expressive and can be compiled into efficient code by means of fusion.

Some application of streams, such as digital filters, rely on having a
buffer containing previous elements of the stream.
Using the functional stream representation means having to copy the
whole buffer when computing a new element of the stream. Such copying
is prohibitively expensive. This paper presents a new representation
of streams which allows for using imperative updates while retaining
the advantages of the functional stream representation. Our
contributions are:

* A new streams representation, `M (M a)`, for some monad `M` which
  supports destructive update. The new representation, while relying
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

## Relation to Functional Streams

It turns out that the functional representation of streams can be
recovered from out new, monadic representation. It is an interesting
excercise because it sheds new light on the monadic represenation.
Consider again the type `M (M a)`. The outer and the inner monads are
the same. But they don't have to be. We could imagine a representation
`M (N a)`. The monad `M` would be responsible for initializing
memory while the monad `N` would be reading and writing that memory.
If we forego mutation, we can let `M a` be `(a,s)` and `N a` be
`(s -> (a,s))`. We recognize them as the writer monad and the state
monad. These two monads, when combined, results in the functional
stream representation.

## Streams for EDSLs

Our new monadic representation of streams is a natural fit for
embedded domain specific languages and work particularly well with
the technique of combining shallow and deep embeddings [@]. Monads can
be embedded in an EDSL using the technique from [@genericmonads11].


# Avoiding multiple loop variables

# Evaluation

# Related work

## FRP

## Conduits and pipes

## Machines

## Future Work

