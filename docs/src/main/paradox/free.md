# free monad implemented with a type-aligned list

This document provides an example of how a free monad could be implemented using a type-aligned
list. It introduces lower-level techniques that can be used with type-aligned lists, such as
destructuring via the @scaladoc[uncons1](maligned.TANonEmptyList#uncons1) method.

This document assumes that the reader is already familiar with the concept of free monads. If you
want an introduction to free monads and their common use-cases, there are a number of resources
available online such as the [Cats free monad documentation][Cats Free docs].

There are many ways to implement free monads in Scala, and this particular implementation is mostly
meant for demonstrative/learning purposes. Having said that, this is a fairly reasonable way to
implement a free monad in Scala. Unlike many naive Scala implementations of free monads, it is
stack-safe (that is, it won't throw a stack overflow exception). It's performance should also be
reasonable (though this should be verified with benchmarks).

## the FreeMonad data structure

We start off by defining an abstract FreeMonad type for an effect type `F[_]` and result value of
type `A`:

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #free-type }

This is a bare-bones `FreeMonad` type and at this point doesn't define anything useful; we'll add
that later.

We'll need some ways to construct a `FreeMonad`. The two most straightforward ways to create a `FreeMonad[F[_], A]` value are by simply wrapping an `A` value (the `Pure` constructor) or an `F[A]` value (the `Suspend` constructor). Both `Pure` and `Suspend` wrap a value without providing a "continuation" function to later apply to the value. We'll make them extend a common `NoCont` (_no continuation_) super-type which will end up being useful later on.

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #no-cont-types }

The third and final way to construct a `FreeMonad` value is to call `flatMap` on a `FreeMonad[F, A]` value, providing a function of type `A => FreeMonad[F, B]` to produce a `FreeMonad[F, B]` value. We'll refer to functions of this `A => FreeMonad[F, B]` form as _continuations_, as they describe a way to continue the computation once an `A` value has been produced. In fact, let's go ahead and define a `Cont` type alias for continuation functions:

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #cont-type }

Now we can define a constructor for free monads that are created via `flatMap`. It will have two
fields:

* The initial `FreeMonad[F, A]` value upon which `flatMap` was called (`init`)
* A non-empty list of continuation functions (`conts`). This will have a single value if `flatMap`
    was only called once, and will have an additional value for each time that `flatMap` is called.

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #flatmapped-class }

The astute reader may have noticed that we used `TANonEmptyList.Rev` as opposed to `TANonEmptyList`
for the continuations. This is an optimization and isn't strictly necessary. Prepending to a `TANonEmptyList`
is more efficient than appending to it (constant time as opposed to linear time), so we prepend to
the continuation list every time that `flatMap` is called, resulting in a reversed list of
continuations. Operations that consume the free monad value will most likely need to reverse the
list, which is a linear-time operation, but better one linear-time operation than many.

## putting the monad in free monad

Now we have all the pieces in place to be able to define a `Monad` instance for `Free`!

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #monad-instance }

## implementing foldMap

A free monad value is only useful if you can consume it. One of the most common ways to consume a
free monad value is with a `foldMap` method that recursively steps through the free monad value,
translating all `F[_]` values into `G[_]` values via a provided `FunctionK[F, G]`. The signature for
`foldMap` looks like this:

```scala
def foldMap[F[_], G[_], A](fa: FreeMonad[F, A])(f2g: FunctionK[F, G])(implicit G: Monad[G]): G[A]
```

Implementing `foldMap` for `NoCont` values is pretty straightforward, so let's start with that:

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #foldMap-no-cont }

Implementing `foldMap` for the `FlatMapped` case in a stack-safe manner is significantly more
complicated. We'll take the approach of keeping track of the current input value and a stack (a
`TANonEmptyList` structure) of continuations that still need to be run. First let's define some
helper type aliases for the (input, continuation) stack pairs:

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #input-and-conts }

We'll utilize these helpers in a function that performs a single "step" of the `foldMap`:

* Pop the next continuation off of the stack.
* Pass the current input value into the popped continuation.
* If any further continuations are returned, prepend them to the stack.
* Find the input value for the next continuation.
* If there are no more continuations, return a `Right` wrapping the final value. Otherwise return a
    `Left` with the remaining continuation stack and the input value for the next continuation.

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #foldMapStep }

Finally we can define `foldMap`:

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #foldMap }

## using foldMap

To convince ourselves that our `foldMap` implementation is correct and stack-safe, we can use a
classic example of using `Function0` as the `F[_]` type to create a [trampoline] and compute a value
via two mutually-recursive functions that would not be stack-safe in the absence of trampolining.

@@snip [[FreeMonad.scala]](/core/src/test/scala/example/FreeMonad.scala) { #trampoline-example }

[Cats Free docs]: https://typelevel.org/cats/datatypes/freemonad.html
[trampoline]: https://medium.com/@olxc/trampolining-and-stack-safety-in-scala-d8e86474ddfa
