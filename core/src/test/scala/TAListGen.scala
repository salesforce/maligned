/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

import cats.implicits._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

import Arbitrary.arbitrary

object TAListGen {

  /**
   * A track of the remaining count of elements and a `Gen` instance.
   */
  private type CountAndGen[A] = (Int, Gen[A])

  /**
   * A generator of tuples of `Gen` and `Cogen` instances for existential types. This can be useful
   * when you want to make sure that your code works for any "random" type given that the type has
   * both `Gen` instance and a `Cogen` instance.
   *
   * [[https://www.youtube.com/watch?v=O78hnJuzQwA This talk]] by Erik Osheim describes a similar
   * approach to generating random types that match a constraint.
   */
  val genGenAndCogen: Gen[TATuple2K[Gen, Cogen]] = Gen.oneOf(
    TATuple2K(arbitrary[Int], Cogen[Int]),
    TATuple2K(arbitrary[Long], Cogen[Long]),
    TATuple2K(arbitrary[Option[Double]], Cogen[Option[Double]]),
    TATuple2K(arbitrary[Boolean], Cogen[Boolean]),
    TATuple2K(arbitrary[String], Cogen[String]),
    TATuple2K(arbitrary[List[Int]], Cogen[List[Int]])
  )

  /**
   * A generator for a non-empty type-aligned list of functions.
   *
   * @param size the number of elements to have in the list (must be at least 1)
   */
  def genFn1TANonEmptyListOfN[A, B](
    size: Int,
    cogenA: Cogen[A],
    genB: Gen[B]): Gen[TANonEmptyList[Function1, A, B]] =
    if (size < 1)
      sys.error("size must be at least 1")
    else if (size === 1)
      Arbitrary.arbFunction1(Arbitrary(genB), cogenA).arbitrary.map(TANonEmptyList.one)
    else
      for {
        // a random type (call it `P`) that has `Gen` and `Cogen` instances that we can use as the
        // input type for the last element of the list. That is, the last element of the list will
        // be a `P => B` function. The `Cogen[P]` instance is required by Scalacheck to create an
        // arbitrary function that takes `P` instances as input.
        initGenAndCogen <- genGenAndCogen
        // the initial (last) function (`P => B`) in the list.
        initFn <- Arbitrary.arbFunction1(Arbitrary(genB), initGenAndCogen.second).arbitrary
        // Create an unfold instance to build up the list.
        // It uses `CountAndGen` as its state (`S`) type to keep track of the remaining number of
        // elements that should be in the list (the "Count"/`Int), and the `Gen[P]` instance for the
        // input type of the current head of the list. This `Gen[P]` instance will be used to generate
        // the previous element of the list, which will be a function with `P` as its output type.
        unfold = new TAUnfoldRightNonEmpty[Function1, CountAndGen, Gen, A, B] {

          // The initial `P => B` function to put at the end of the list, along with the updated
          // remaining element count and the `Gen[P]` instance.
          override def init: TATuple2K[CountAndGen, * => B] =
            TATuple2K[CountAndGen, * => B, initGenAndCogen.A](
              (size - 1, initGenAndCogen.first),
              initFn)

          override def prev[X, Y](
            s: CountAndGen[X],
            f: X => Y): Gen[Either[A => X, TATuple2K[CountAndGen, * => X]]] = {
            val (remainingElementCnt, genX) = s
            if (remainingElementCnt === 1)
              // This is the head of the final list. It should be a function that takes `A` as its
              // input type. Return a `Left` to terminate the unfold.
              Arbitrary.arbFunction1(Arbitrary(genX), cogenA).arbitrary.map(_.asLeft)
            else
              // There are still more elements needed. Generate a random type (that has `Gen` and
              // `Cogen` instances), generate a function that takes this type as input, prepend the
              // function to the list, and return a `Right` to continue the unfold.
              for {
                genCogen <- genGenAndCogen
                f <- Arbitrary.arbFunction1(Arbitrary(genX), genCogen.second).arbitrary
              } yield TATuple2K[CountAndGen, * => X, genCogen.A](
                (remainingElementCnt - 1, genCogen.first),
                f).asRight
          }
        }
        unfolded <- TANonEmptyList.unfoldM(unfold)
      } yield unfolded

  /**
   * Similar to [[genFn1TANonEmptyListOfN]], but since the list can be empty, the input type must
   * match the output type.
   */
  def genFn1TAListOfN[A](size: Int, cogenA: Cogen[A], genA: Gen[A]): Gen[TAList[Function1, A, A]] =
    if (size < 0)
      sys.error("size cannot be negative")
    else if (size === 0)
      Gen.const(TAList.nil)
    else
      genFn1TANonEmptyListOfN[A, A](size, cogenA, genA)

  /**
   * An `Arbitrary` instance wrapping [[genFn1TANonEmptyListOfN]].
   */
  implicit def arbFn1TANonEmptyList[A, B](implicit
    cogenA: Cogen[A],
    arbB: Arbitrary[B]): Arbitrary[TANonEmptyList[Function1, A, B]] = {
    val gen = Gen.sized(maxSize =>
      Gen
        .choose(1, math.max(1, maxSize))
        .flatMap(size => genFn1TANonEmptyListOfN(size, cogenA, arbB.arbitrary)))
    Arbitrary(gen)
  }

  /**
   * An `Arbitrary` instance wrapping [[genFn1TAListOfN]].
   */
  implicit def arbFn1TAList[A](implicit
    cogenA: Cogen[A],
    arbA: Arbitrary[A]): Arbitrary[TAList[Function1, A, A]] = {
    val gen = Gen.sized(maxSize =>
      Gen
        .choose(0, maxSize)
        .flatMap(size => genFn1TAListOfN(size, cogenA, arbA.arbitrary)))
    Arbitrary(gen)
  }
}
