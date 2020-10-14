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
import cats.data.Kleisli
import cats.effect.IO
import cats.implicits._
import com.salesforce.maligned._

import java.time.Duration
// #imports

// #stage-type
trait Stage[A, B] {
  def name: String

  def apply(input: A): IO[B]
}
// #stage-type

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
class DataPipelineExample extends MalignedSpec {

  // #nlp-stages
  /**
   * Normalizes the case of characters in the input string.
   *
   * This simple version just converts everything to lower case.
   */
  val normalizeCase = new Stage[String, String] {
    def name: String = "normalize-case"

    def apply(input: String): IO[String] = IO.pure(input.toLowerCase)
  }

  /** A logical grouping of characters — usually a word */
  type Token = String

  /** Splits an input string into a sequence of tokens (words) */
  val tokenize = new Stage[String, List[Token]] {
    def name: String = "tokenize"

    def apply(input: String): IO[List[Token]] = IO.pure(input.split("""\s""").toList)
  }

  /** Counts the number of occurrences of each token in the input sequence of tokens. */
  val termFrequency = new Stage[List[Token], Map[Token, Int]] {
    def name: String = "term-frequency"

    def apply(input: List[Token]): IO[Map[Token, Int]] =
      IO.pure(input.foldMap(token => Map(token -> 1)))
  }
  // #nlp-stages

  // #pipeline-type
  type Pipeline[A, B] = TAList[Stage, A, B]
  // #pipeline-type

  // #bag-of-words-pipeline
  val bagOfWordsPipeline: Pipeline[String, Map[Token, Int]] =
    normalizeCase ::
      tokenize ::
      TANonEmptyList.one(termFrequency)
  // #bag-of-words-pipeline

  // #pretty-print-stage-names
  def prettyPrintStageNames[A, B](pipeline: Pipeline[A, B]): String =
    pipeline.toList.map(_.name).mkString_("->")

  val bagOfWordsStageNames = prettyPrintStageNames(bagOfWordsPipeline)
  assert(bagOfWordsStageNames eqv "normalize-case->tokenize->term-frequency")
  // #pretty-print-stage-names

  // #stage-to-kleisli
  val stageToKleisli: FunctionK2[Stage, Kleisli[IO, *, *]] =
    new FunctionK2[Stage, Kleisli[IO, *, *]] {
      def apply[A, B](stage: Stage[A, B]): Kleisli[IO, A, B] = Kleisli(stage.apply(_))
    }

  val kleisliList: TAList[Kleisli[IO, *, *], String, Map[Token, Int]] =
    bagOfWordsPipeline.mapK(stageToKleisli)
  // #stage-to-kleisli

  // #composed-pipeline
  val toBagOfWords: Kleisli[IO, String, Map[Token, Int]] = kleisliList.composeAll

  val output: Map[Token, Int] = toBagOfWords("I think therefore I am").unsafeRunSync
  assert(output eqv Map("i" -> 2, "think" -> 1, "therefore" -> 1, "am" -> 1))
  // #composed-pipeline

  // #time-io
  def timeIO[A](action: IO[A], reportTime: (Duration, Option[Throwable]) => IO[Unit]): IO[A] =
    for {
      start <- IO(System.nanoTime)
      result <- action.attempt
      end <- IO(System.nanoTime)
      _ <- reportTime(Duration.ofNanos(end - start), result.left.toOption)
      a <- IO.fromEither(result)
    } yield a
  // #time-io

  // #logged-stage
  val loggedStage = new FunctionK2[Stage, Kleisli[IO, *, *]] {

    def apply[A, B](stage: Stage[A, B]): Kleisli[IO, A, B] = {
      def msg(dur: Duration, err: Option[Throwable]): String =
        s"${stage.name} ${err.fold("succeeded")(_ => "failed")} in ${dur.toNanos} nanoseconds"

      Kleisli { input =>
        timeIO(stage(input), (duration, err) => IO(println(msg(duration, err))))
      }
    }
  }
  // #logged-stage

  // #timed-bag-of-words
  val timedBagOfWords: Kleisli[IO, String, Map[Token, Int]] =
    bagOfWordsPipeline.mapK(loggedStage).composeAll

  timedBagOfWords("I think therefore I am")
    .flatMap(counts => IO(println(counts.show)))
    .unsafeRunSync
  // normalize-case succeeded in 8355 nanoseconds
  // tokenize succeeded in 4018 nanoseconds
  // term-frequency succeeded in 4405 nanoseconds
  // Map(am -> 1, therefore -> 1, think -> 1, i -> 2)
  // #timed-bag-of-words

  // #timed-bag-of-words-no-talist
  val timedBagOfWordsNoTAList: Kleisli[IO, String, Map[Token, Int]] =
    loggedStage(normalizeCase) andThen
      loggedStage(tokenize) andThen
      loggedStage(termFrequency)
  // #timed-bag-of-words-no-talist
}

// #stage2-type
trait Stage2[A, B] {
  def name: String

  def apply(input: A): IO[B]

  def deserializeInput(input: Array[Byte]): Either[Exception, A]

  def serializeOutput(output: B): Either[Exception, Array[Byte]]
}
// #stage2-type
