/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned
package gen

import cats.implicits._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

import Arbitrary.arbitrary

/**
 * Supports generating pseudorandom [[TAList]] structures.
 *
 * If you create a [[TAListGenerator]] instance for a type, you can then use the methods in
 * [[TAListGen]] to create `org.scalacheck.Gen` instances for type-aligned lists of that type.
 *
 * @tparam F the effect type of the type-aligned list (for example `Function1`)
 * @tparam S a type encapsulating any state required to generate the preceding element in the list
 * @tparam L the input type of the generated list
 * @tparam R the output type of the generated list
 */
trait TAListGenerator[F[_, _], S[_], L, R] { generator =>

  /**
   * A generator with input type `L` and output type `R` that can be used for single-element lists.
   */
  def single: Gen[F[L, R]]

  /**
   * A generator for the last element in generated lists.
   */
  def last: Gen[TATuple2K[S, F[*, R]]]

  /**
   * A generator for the preceding element in a list, given the following element and the current
   * state.
   *
   * @see [[TAListGenerator.fn1TAListGenerator]] for example usage
   */
  def prev[P, Y](state: S[P], next: F[P, Y]): Gen[TATuple2K[S, F[*, P]]]

  /**
   * A generator for the head (far left) element in generated lists.
   */
  def head[P, Y](state: S[P], next: F[P, Y]): Gen[F[L, P]]

  /**
   * A generator for non-empty type aligned lists.
   *
   * @param size the size of the generated list. NOTE: an exception will be thrown if `size` is
   * less than 1.
   */
  def sizedNelGen(size: Int): Gen[TANonEmptyList[F, L, R]] =
    if (size < 1)
      sys.error("size must be at least 1")
    else if (size === 1)
      single.map(TANonEmptyList.one(_))
    else {
      type CountAndState[A] = (Int, S[A])
      last.flatMap { last =>
        val unfold = new TAUnfoldRightNonEmpty[F, CountAndState, Gen, L, R] {

          // The initial element to put at the end of the list, along with the updated remaining
          // element count and state.
          override def init: TATuple2K[CountAndState, F[*, R]] =
            TATuple2K[CountAndState, F[*, R], last.A]((size - 1, last.first), last.second)

          override def prev[X, Y](
            s: CountAndState[X],
            f: F[X, Y]): Gen[Either[F[L, X], TATuple2K[CountAndState, F[*, X]]]] = {
            val (remainingElementCnt, state) = s
            if (remainingElementCnt === 1)
              // This is the head of the final list. It has `A` as its input type. Return a `Left`
              // to terminate the unfold.
              head(state, f).map(_.asLeft)
            else
              // There are still more elements needed. Generate the previous element and state and
              // return a `Right` to continue the unfold.
              generator.prev(state, f).map { p =>
                TATuple2K[CountAndState, F[*, X], p.A](
                  (remainingElementCnt - 1, p.first),
                  p.second).asRight
              }
          }
        }
        TANonEmptyList.unfoldM(unfold)
      }
    }

  /**
   * Similar to [[sizedNelGen]], but the length is chosen randomly.
   */
  def nelGen: Gen[TANonEmptyList[F, L, R]] =
    Gen.sized(maxSize =>
      Gen
        .choose(1, math.max(1, maxSize))
        .flatMap(size => sizedNelGen(size)))
}

object TAListGenerator {

  def listOfNGen[F[_, _], S[_], A](
    size: Int,
    generator: TAListGenerator[F, S, A, A]): Gen[TAList[F, A, A]] =
    if (size < 0)
      sys.error("size cannot be negative")
    else if (size === 0)
      Gen.const(TAList.nil)
    else
      generator.sizedNelGen(size)

  /**
   * A generator of tuples of `Gen` and `Cogen` instances for existential types. This can be useful
   * when you want to make sure that your code works for any "random" type given that the type has
   * both `Gen` instance and a `Cogen` instance.
   *
   * [[https://www.youtube.com/watch?v=O78hnJuzQwA This talk]] by Erik Osheim describes a similar
   * approach to generating random types that match a constraint.
   */
  val genGenAndCogen: Gen[TATuple2K[Gen, Cogen]] = Gen.frequency(
    1 -> TATuple2K(arbitrary[Unit], Cogen[Unit]),
    2 -> TATuple2K(arbitrary[Boolean], Cogen[Boolean]),
    5 -> TATuple2K(arbitrary[String], Cogen[String]),
    5 -> TATuple2K(arbitrary[Option[Double]], Cogen[Option[Double]]),
    5 -> TATuple2K(arbitrary[List[Int]], Cogen[List[Int]]),
    3 -> TATuple2K(arbitrary[Either[String, Int]], Cogen[Either[String, Int]]),
    5 -> TATuple2K(arbitrary[Either[Boolean, Byte]], Cogen[Either[Boolean, Byte]]),
    10 -> TATuple2K(arbitrary[Long], Cogen[Long]),
    15 -> TATuple2K(arbitrary[Int], Cogen[Int])
  )

  def fn1TAListGenerator[A, B](
    cogenA: Cogen[A],
    genB: Gen[B]): TAListGenerator[Function1, Gen, A, B] =
    new TAListGenerator[Function1, Gen, A, B] {
      override def single: Gen[A => B] = Arbitrary.arbFunction1(Arbitrary(genB), cogenA).arbitrary

      override def last: Gen[TATuple2K[Gen, * => B]] =
        for {
          genAndCogen <- genGenAndCogen
          initFn <- Arbitrary.arbFunction1(Arbitrary(genB), genAndCogen.second).arbitrary
        } yield TATuple2K[Gen, * => B, genAndCogen.A](genAndCogen.first, initFn)

      override def prev[P, Y](state: Gen[P], next: P => Y): Gen[TATuple2K[Gen, * => P]] =
        for {
          genCogen <- genGenAndCogen
          f <- Arbitrary.arbFunction1(Arbitrary(state), genCogen.second).arbitrary
        } yield TATuple2K[Gen, * => P, genCogen.A](genCogen.first, f)

      override def head[P, Y](state: Gen[P], next: P => Y): Gen[A => P] =
        Arbitrary.arbFunction1(Arbitrary(state), cogenA).arbitrary
    }
}
