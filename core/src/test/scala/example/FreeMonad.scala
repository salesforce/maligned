/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned.example

import com.salesforce.maligned._

import cats._
import cats.arrow.FunctionK
import cats.implicits._

// #free-type
sealed abstract class FreeMonad[F[_], A]
// #free-type

object FreeMonad {
  // #no-cont-types
  /**
   * A free monad that wraps a value and does not provide any "continuation" functions to apply to
   * the value
   */
  sealed abstract class NoCont[F[_], A] extends FreeMonad[F, A]

  /** A pure `A` value */
  final case class Pure[F[_], A](value: A) extends NoCont[F, A]

  /** An `A` value in an `F` effect context */
  final case class Suspend[F[_], A](value: F[A]) extends NoCont[F, A]

  def pure[F[_], A](value: A): FreeMonad[F, A] = Pure(value)

  def suspend[F[_], A](value: F[A]): FreeMonad[F, A] = Suspend(value)
  // #no-cont-types

  // #cont-type
  /** A continuation that takes an A value and returns a new FreeMonad to continue the computation */
  type Cont[F[_], A, B] = A => FreeMonad[F, B]
  // #cont-type

  // #flatmapped-class
  final private case class FlatMapped[F[_], A, B](
    init: NoCont[F, A],
    conts: TANonEmptyList.Rev[Cont[F, *, *], A, B])
      extends FreeMonad[F, B]
  // #flatmapped-class

  // #monad-instance
  implicit def monadForFreeMonad[F[_]]: Monad[FreeMonad[F, *]] =
    new StackSafeMonad[FreeMonad[F, *]] {

      override def pure[A](x: A): FreeMonad[F, A] = Pure(x)

      override def flatMap[A, B](fa: FreeMonad[F, A])(f: A => FreeMonad[F, B]): FreeMonad[F, B] =
        fa match {
          case x: NoCont[F, A] => FlatMapped(x, TANonEmptyList.revOne[Cont[F, *, *], A, B](f))
          case FlatMapped(init, conts) => FlatMapped(init, f :: conts)
        }
    }
  // #monad-instance

  // #foldMap-no-cont
  def foldMapNoCont[F[_], G[_], A](fa: NoCont[F, A])(f2g: FunctionK[F, G])(implicit
    G: Monad[G]): G[A] =
    fa match {
      case Pure(value) => G.pure(value)
      case Suspend(value) => f2g(value)
    }
  // #foldMap-no-cont

  // #input-and-conts
  /**
   * A stack of continuations.
   *
   * @tparam I the input type of the first continuation
   * @tparam O the output type of the last continuation
   */
  type Conts[F[_], I, O] = TANonEmptyList[Cont[F, *, *], I, O]

  /**
   * A stack of continuations along with the input for the first continuation.
   *
   * This is essentially a tuple `(I, Conts[F, I, O])` of an input `I` value and a list of
   * continuations to pass the input value into. `Tuple2K` is used instead of a vanilla tuple,
   * because the `I` type is an unknown/existential type; `Tuple2K` ensures that it lines up between
   * the input value and the first continuation.
   */
  type InputAndConts[F[_], O] = TATuple2K[Id, Conts[F, *, O]]

  /** Simple helper method to create `InputAndConts` values */
  @inline def makeInputAndConts[F[_], I, A](input: I, conts: Conts[F, I, A]): InputAndConts[F, A] =
    TATuple2K[Id, Conts[F, *, A], I](input, conts)
  // #input-and-conts

  // Pop a continuation off of the stack and apply it to the input value.
  // If there is nothing left on the stack, return an `A` value in a `Right`.
  // Otherwise return a `Left` value wrapping the remaining stack of continuations and the
  // input argument to pass into the next continuation.
  // #foldMapStep
  def foldMapStep[F[_], G[_], A](inputAndConts: InputAndConts[F, A])(f2g: FunctionK[F, G])(implicit
    G: Monad[G]): G[Either[InputAndConts[F, A], A]] =
    inputAndConts.second.uncons1 match {
      // last continuation on the stack
      case Left(cont) =>
        // run the continuation on the input value
        cont(inputAndConts.first) match {
          // nothing to add to the stack; we are done
          case y: NoCont[F, A] => foldMapNoCont(y)(f2g).map(Right(_))
          // new continuations to add to the stack
          case FlatMapped(init, conts) =>
            foldMapNoCont(init)(f2g).map(x => Left(makeInputAndConts(x, conts.reverse)))
        }
      // multiple continuations on the stack
      case Right(conts) =>
        // run the first continuation on the input value
        conts.head(inputAndConts.first) match {
          // the result doesn't add any continuations to the stack
          case y: NoCont[F, conts.P] =>
            foldMapNoCont(y)(f2g).map(x => Left(makeInputAndConts(x, conts.tail)))
          // the result adds continuations on the stack
          case FlatMapped(init, additionalConts) =>
            foldMapNoCont(init)(f2g).map(x =>
              Left(makeInputAndConts(x, conts.tail.prependReversed(additionalConts))))
        }
    }
  // #foldMapStep

  // #foldMap
  def foldMap[F[_], G[_], A](fa: FreeMonad[F, A])(f2g: FunctionK[F, G])(implicit
    G: Monad[G]): G[A] =
    fa match {
      // simple case of no continations; we are done!
      case x: NoCont[F, A] => foldMapNoCont(x)(f2g)
      // continuations exist; we'll need to step through the stack of continuations one at a time
      case FlatMapped(init, conts) =>
        // calculate the initial value and then recursively `step` through the continuation stack
        foldMapNoCont(init)(f2g).flatMap(v =>
          G.tailRecM(makeInputAndConts(v, conts.reverse))(foldMapStep(_)(f2g)))
    }
  // #foldMap
}

class FreeMonadExamples extends MalignedSpec {
  // #trampoline-example
  type Trampoline[A] = FreeMonad[Function0, A]

  def defer[A](delayed: () => Trampoline[A]): Trampoline[A] =
    FreeMonad.pure[Function0, Unit](()).flatMap(_ => delayed())

  def even(n: Int): Trampoline[Boolean] =
    if (n eqv 0) FreeMonad.pure(true) else defer(() => odd(n - 1))

  def odd(n: Int): Trampoline[Boolean] =
    if (n eqv 0) FreeMonad.pure(false) else defer(() => even(n - 1))

  val evaluateFunction0: FunctionK[Function0, Id] = new FunctionK[Function0, Id] {
    def apply[A](value: Function0[A]): A = value()
  }

  def evalTrampoline[A](value: Trampoline[A]): A = FreeMonad.foldMap(value)(evaluateFunction0)

  assert(evalTrampoline(even(100000)))
  assert(!evalTrampoline(even(100001)))
  // #trampoline-example
}
