# type-aligned list generation

This document provides examples of how you can generate pseudorandom (but deterministic) type-aligned lists using maligned and Scalacheck.

## prerequisites

The code examples assume that the following items have been imported:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #imports }

They also depend on the following helper method to sample a Scalacheck `Gen` instance with the provided RNG seed:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #sample }

## generating random type-aligned lists of functions

Importing @scaladoc[TAListGen._](maligned.gen.TAListGen$) provides implicit Scalacheck `Arbitrary` instances for type-aligned lists (@scaladoc[TANonEmptyList](maligned.TANonEmptyList) and @scaladoc[TAList](maligned.TAList)) of function (`Function1`) elements.

For example, we can create a generator for lists of length 1 to 6 with an input type of `Int` and an output type of `Option[Double]`:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #taListGen }

Let's go ahead and sample a random list from this generator:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #exampleList }

We can compose this list into a single function:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #composedFunction }

But this doesn't give us much insight into the internal structure of the list. Is it just a single element that is a function from `Int` to `Option[Double]`? We can see that it's not by checking the length of the list:

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #exampleListSize }

To get a better understanding of the composition of the list, we'll have to get a little fancier. We can wrap each function element in some logic to log its output. This will allow us to see all of the intermediate values of the composed functions.

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #withLogging }

As you can see, the list was composed of functions of various types. Because the RNG is deterministic, if we run the function again with the same input value, we'll get the same output. But if we change the input value, we'll see completely different intermediate values.

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #withLoggingAgain }

If we generate a new list, it will be comprised of completely different functions (with different inner input/output types):

@@snip [[TAListGenExample.scala]](/core-tests/src/test/scala/example/TAListGenExample.scala) { #exampleList2 }
