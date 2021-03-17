# type-aligned data pipeline

This document provides an example of how one could use a type-aligned sequence (specifically @scaladoc[TAList](maligned.TAList)) to represent data pipelines. It will introduce some of the useful high-level methods on type-aligned lists.

The code examples assume that the following items have been imported:

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #imports }

## pipeline stages

A data pipeline is a sequence of stages, where each stage takes an input value and transforms it into an output value. For this example we'll also declare that every stage has a name and can perform arbitrary IO when producing its output value.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #stage-type }

Now we can define a number of (slightly simplified) stages that you might find in a natural language processing (NLP) pipeline. At this point none of these stages perform side effects so the `IO` in the return type isn't necessary, but other stages might perform side effects so we'll leave it open as a possibility.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #nlp-stages }

## putting the stages together

Now that we have defined stages, we can define a data pipeline as a type-aligned list (@scaladoc[TAList](maligned.TAList)) of stages.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #pipeline-type }

`A` is the type of input data that is fed into the first stage of the pipeline and `B` is the type of output data returned by the last stage of the pipeline.

Now let's create a "bag of words" text data pipeline. Treating text as a "bag of words" means that you simply count the number of times that each word occurs, ignoring the order of words completely. This approach can be surprisingly effective for certain tasks.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #bag-of-words-pipeline }

As you can see, we use @scaladoc[TANonEmptyList.one](maligned.TANonEmptyList$#one) to wrap the last element in a type-aligned list and use `::` to prepend elements to it. This is analogous to using `1 :: 2 :: NonEmptyList.one(3)` to construct a [Cats NonEmptyList][NonEmptyList]

We have a data pipeline! Read on to find out what we can do with it.

## operations on type-aligned lists

Type-aligned lists have a number of operations that mirror standard list operations (such as `map`, `foldLeft`, etc), but the type-aligned versions are generally a bit more complex.

### toList

Sometimes it can be handy to avoid complexity by throwing away type information and converting a type-aligned list into a standard `List` that has unknown input and output types. For example, since each stage has a `name` field that is a `String` regardless of the input and output types of the stage, we can write a method to pretty-print the stage names using @scaladoc[toList](maligned.TAList#toList).

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #pretty-print-stage-names }

### mapK

The `map` method on a traditional `List[A]` takes a function `A => B` and transforms the list into a `List[B]` by running the function on each element in the list. What would it mean for a type-aligned list to have a `map` method? Consider a type-aligned list with three elements, `F[A => B], F[B => C], F[C => D]`. How could we possibly pass a single function in that could act upon every element in the list? A traditional function won't work for this. Since `A`, `B`, `C`, and `D` could all be different types, we need to ensure that the "function" that we pass in can handle `F[X, Y]` for _all_ possible `X` and `Y` types. The input to the function could look like `def apply[X, Y](f: F[X, Y])` to force it to work for _all_ `X` and `Y` types, but what would the return type be? If we want to maintain the type-aligned nature of the sequence then the function can't change the `X` and `Y` type parameters, but it _can_ transform `F` to another type: `def apply[X, Y](f: F[X, Y]): G[X, Y]`. This signature is provided by @scaladoc[FunctionK2](maligned.FunctionK2) which is similar to the [FunctionK type in Cats][FunctionK], but `FunctionK` works on types of the shape `F[_]` (like `Option`) while @scaladoc[FunctionK2](maligned.FunctionK2) works on types of the shape `F[_, _]` (like `Either`).

We can define a `FunctionK2` that transforms each `Stage` into a [`Kleisli`][Kleisli], and then we can then pass the `FunctionK` instance into @scaladoc[mapK](maligned.TAList#mapK).

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #stage-to-kleisli }

### composeAll

At this point it might not be clear why converting from a `Stage` to a `Kleisli` might be useful. One nice property of the `Kleisli` type is that it forms a [`Category`][Category], so a `Kleisli[IO, A, B]` can be composed with a `Kleisli[IO, B, C]` to form a `Kleisli[IO, A, C]` that feeds the output of the first `Kleisli` as input into the second `Kleisli` and returns the result of the second `Kleisli`.

`TAList[F, A, B]` has a @scaladoc[composeAll](maligned.TAList#composeAll) method that will use the `Category` instance for `F` to compose all of the elements of the list into a single `F[A, B]`. In this case it can compose our `Kleisli` pipeline into a single `Kleisli`

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #composed-pipeline }

## making the pipeline more useful

So far we haven't gotten much of a benefit from using a type-aligned list of `Stage` elements for our pipeline. We are able to pretty-print the steps of the pipeline which may be a bit useful but probably doesn't justify the introduction of type-aligned sequences. The effort starts to pay off when we want to add common logic to every stage in a pipeline. For example, when we convert a `Stage` to a `Kleisli`, we could make that `Kleisli` automatically log the name of the stage and the amount of time that it took to run for each input.

First we'll define a general method for measuring the amount of time that an `IO` operation takes to run.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #time-io }

Now we can define a new `Stage` to `Kleisli` transformation that performs the timing and logging.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #logged-stage }

Now that all of the pieces are in place, we can compose a single `Kleisli` that will run the entire pipeline, timing and logging each stage as it is executed.

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #timed-bag-of-words }

By maintaining a type-aligned sequence of stages, we were able to make a single `mapK` call to apply this common logic to _every_ stage. If we hadn't used a type-aligned list, adding logging and timing to each step would have become significantly more repetitive:

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #timed-bag-of-words-no-talist }

For a 3-stage pipeline this might not be too bad, but as the number of stages in the pipeline grows, so will the benefit of using a type-aligned sequence.

## is Stage useful?

We ended up converting all of our `Stage` elements into `Kleisli` elements, so it's reasonable to question whether we should have used `Kleisli` directly and omitted the `Stage` type entirely. We can't really beat `Kleisli` if our goal is strictly to compose together effectful functions. However, function composition might not be the only task that we want to accomplish with our pipeline. The `prettyPrintStageNames` function that we wrote earlier depended on `Stage` being a type that we could introspect to grab the `name`; `Kleisli[F, A, B]` is an opaque function that we can't get any information out of unless we have an `A` to pass into it.

We could also add more fields to `Stage` to make it even more powerful. For example, we could add input deserializers and output serializers to a `Stage`:

@@snip [[DataPipelineExample.scala]](/core-tests/src/test/scala/example/DataPipelineExample.scala) { #stage2-type }

We could still convert a pipeline into a `Kleisli[IO, A, B]` that completely ignores the serializers like we did before. But we could also implement a more sophisticated runtime for the pipeline that measures how long individual stages tend to take and determines whether to run stages within the same process or serialize the data and add it to a queue to be processed in a distributed manner.

[Category]: https://typelevel.org/cats/api/cats/arrow/Category.html
[FunctionK]: https://typelevel.org/cats/datatypes/functionk.html
[Kleisli]: https://typelevel.org/cats/datatypes/kleisli.html
[NonEmptyList]: https://typelevel.org/cats/datatypes/nel.html
