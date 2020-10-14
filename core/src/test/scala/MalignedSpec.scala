/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

import cats.tests.StrictCatsEquality
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * A base class for Maligned tests.
 *
 * This uses scalatest but sets up scalacheck integration (with some reasonable defaults).
 * It also uses `StrictCatsEquality` to change the `should ===(...)` and `should !==(...)`
 * scalatest matchers to use the Cats `Eq` type class for equality. That means if you use these
 * matchers, you'll need to have the relevant `Eq` instance in implicit scope (usually achieved by
 * importing `cats.implicits._`).
 */
abstract class MalignedSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with StrictCatsEquality {

  // `PosInt.apply` and friends seem to be macros that expand into an `Option.get` call
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  /**
   * Since our tests run very often (every PR, etc), we can reduce the number of successful checks
   * that we require in the interest of speeding up test time. Since each run of tests generates a
   * new RNG seed, over time we still hit lots of test-cases.
   */
  def defaultGeneratorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(30),
      maxDiscardedFactor = PosZDouble(5.0),
      minSize = PosZInt(0),
      sizeRange = PosZInt(30),
      workers = PosInt(2))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    defaultGeneratorDrivenConfig
}
