# maligned

@@@ index

* @ref:[data pipline](data-pipeline.md)
* @ref:[free monad](free.md)

@@@

Maligned is a library for type-aligned sequences.

# type-aligned sequences

A type-aligned sequence is a collection of elements of the form `F[A, B], F[B, C], ... F[C, D]` where the output type of each element lines up with the input type of the next element. The type of the sequence maintains the initial input type (`A` in this example) and the final output type (`D` in this example). All of the intermediate types are guaranteed to line up, but are represented as existential types, meaning that the type exists and is consistent between nodes but the type is unknown. A common case for a type aligned sequence is when `F` is `Function1`, so the above example would be `A => B, B => C, ... C => D`. See @ref:[type-aligned data pipline](data-pipeline.md) for an example of representing a data pipeline as a type-aligned sequence of functions.

Type-aligned sequences support many traditional collection operations, but operations such as folds and reduces require a slightly more complicated type signature to support unknown types in the middle of the sequence.

@scaladoc[TAList](maligned.TAList) is a cons list (like a traditional `scala.collection.immutable.List` with `Cons` `Nil` constructors), but supports type-aligned sequences. A @scaladoc[TAList[F, A, B]](maligned.TAList) is a type-aligned cons list where the first element is of type `F[A, X] forSome X` and the last element is of type `F[Y, B] forSome Y`.

@scaladoc[TANonEmptyList[F, A, B]](maligned.TANonEmptyList) is a @scaladoc[TAList[F, A, B]](maligned.TAList) that is guaranteed to not be empty.

# resources

The source code for Maligned lives [here on GitHub]($sourceUrl$).

# credits and related projects

Maligned was inspired by Tomas Mikula's [Type-Aligned](https://github.com/TomasMikula/type-aligned) library (MPL-2.0 License) and Alexander Konovalov's [taseq](https://github.com/alexknvl/taseq) library (MIT License).

The implementation of the type-aligned sequence data structures (and specifically its stack-safe approach to folds) is heavily based on code in Type-Aligned. One difference between Type-Aligned and maligned is malign's support for flexible left and right folds through its `TAFoldLeft` and `TAFoldRight` types. The `TAFoldLeft` and `TAFoldRight` types were inspired by taseq but have been made a bit more complex to allow them to support a wider variety of folds.
