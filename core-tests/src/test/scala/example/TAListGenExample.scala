/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned.example

import com.salesforce.maligned.MalignedSpec

// OrganizeInputs keeps trying to delete the #imports label 😖
// scalafix:ok OrganizeImports
// #imports
import com.salesforce.maligned._
import com.salesforce.maligned.gen.TAListGen._
import cats.data.{Chain, Kleisli, Writer}
import cats.implicits._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
// #imports

@SuppressWarnings(
  Array(
    "org.wartremover.warts.PublicInference",
    "org.wartremover.warts.ToString",
    "org.wartremover.warts.OptionPartial",
    "org.wartremover.warts.NonUnitStatements"
  ))
class TAListGenExample extends MalignedSpec {
  // #sample
  def sample[A](gen: Gen[A], seed: Seed): A = gen.apply(Gen.Parameters.default, seed).get
  // #sample

  test("Int => Option[Double] example") {

    // #taListGen
    val taListGen: Gen[TANonEmptyList[Function1, Int, Option[Double]]] =
      Gen.resize(6, arbitrary[TANonEmptyList[Function1, Int, Option[Double]]])
    // #taListGen

    // #exampleList
    val exampleList: TANonEmptyList[Function1, Int, Option[Double]] = sample(taListGen, Seed(13L))
    // #exampleList

    // #composedFunction
    val f: Int => Option[Double] = exampleList.composeAll

    f(3) should ===(Some(-8.952315155229148e-202))
    // #composedFunction

    // #exampleListSize
    exampleList.toList.size should ===(6)
    // #exampleListSize

    // #withLogging
    val loggingFn1 = new FunctionK2[Function1, Kleisli[Writer[Chain[String], *], *, *]] {
      override def apply[A, B](f: A => B): Kleisli[Writer[Chain[String], *], A, B] =
        Kleisli { a =>
          val output = f(a)
          Writer(Chain(s"output: ${output.toString}"), output)
        }
    }

    val exampleLoggedList
      : TANonEmptyList[Kleisli[Writer[Chain[String], *], *, *], Int, Option[Double]] =
      exampleList.mapK(loggingFn1)

    val fnWithLogging = exampleLoggedList.composeAll

    fnWithLogging(3).run should ===(
      (
        Chain(
          "output: -1",
          "output: -1809610947",
          "output: List(-1, 1809634624, 2147483647, -53758633)",
          "output: -4249568764532995907",
          "output: -9223372036854775808",
          "output: Some(-8.952315155229148E-202)"
        ),
        Some(-8.952315155229148e-202)))
    // #withLogging

    // #withLoggingAgain
    fnWithLogging(3).run should ===(
      (
        Chain(
          "output: -1",
          "output: -1809610947",
          "output: List(-1, 1809634624, 2147483647, -53758633)",
          "output: -4249568764532995907",
          "output: -9223372036854775808",
          "output: Some(-8.952315155229148E-202)"
        ),
        Some(-8.952315155229148e-202)))

    fnWithLogging(4).run should ===(
      (
        Chain(
          "output: 1929283752716877934",
          "output: 1",
          "output: List(160958044, 1601040997, -2147483648, -139952998, 1)",
          "output: 7163398877903029397",
          "output: 7118195307056659999",
          "output: Some(1.0051322294659681E109)"
        ),
        Some(1.0051322294659681e109)))
    // #withLoggingAgain

    // #exampleList2
    val exampleList2: TANonEmptyList[Function1, Int, Option[Double]] =
      sample(taListGen, Seed(1000L))

    val exampleLoggedList2
      : TANonEmptyList[Kleisli[Writer[Chain[String], *], *, *], Int, Option[Double]] =
      exampleList2.mapK(loggingFn1)

    val fn2WithLogging = exampleLoggedList2.composeAll

    fn2WithLogging(3).run should ===(
      (
        Chain(
          "output: 8106783551773268132",
          "output: 0",
          "output: 㷱襈鯡䐌",
          "output: Left(false)",
          "output: Some(9.600598437719758E264)"),
        Some(9.600598437719758e264)))
    // #exampleList2
  }
}
