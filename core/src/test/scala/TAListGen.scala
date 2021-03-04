/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

import org.scalacheck.{Arbitrary, Cogen, Gen}

import TAListGenerator._

object TAListGen {

  /**
   * A generator for a non-empty type-aligned list of functions.
   *
   * @param size the number of elements to have in the list (must be at least 1)
   */
  def genFn1TANonEmptyListOfN[A, B](
    size: Int,
    cogenA: Cogen[A],
    genB: Gen[B]): Gen[TANonEmptyList[Function1, A, B]] =
    fn1TAListGenerator(cogenA, genB).sizedNelGen(size)

  /**
   * Similar to [[genFn1TANonEmptyListOfN]], but since the list can be empty, the input type must
   * match the output type.
   */
  def genFn1TAListOfN[A](size: Int, cogenA: Cogen[A], genA: Gen[A]): Gen[TAList[Function1, A, A]] =
    listOfNGen(size, fn1TAListGenerator(cogenA, genA))

  def genTANonEmptyList[F[_, _], S[_], A, B](
    generator: TAListGenerator[F, S, A, B]): Gen[TANonEmptyList[F, A, B]] =
    Gen.sized(maxSize =>
      Gen
        .choose(1, math.max(1, maxSize))
        .flatMap(size => generator.sizedNelGen(size)))

  /**
   * An `Arbitrary` instance wrapping [[genFn1TANonEmptyListOfN]].
   */
  implicit def arbFn1TANonEmptyList[A, B](implicit
    cogenA: Cogen[A],
    arbB: Arbitrary[B]): Arbitrary[TANonEmptyList[Function1, A, B]] = {
    val gen = genTANonEmptyList(fn1TAListGenerator(cogenA, arbB.arbitrary))
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
