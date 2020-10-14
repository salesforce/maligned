/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

/**
 * Equivalent to `cats.data.Tuple2K` but uses an existential type parameter for the pivot (`A`) type. This
 * allows instances that share the same `F` and `G` types to be considered the same type, even if
 * they don't share the same pivot type. This feature is being used to enable tail-recursive
 * traversal across type-aligned structures such as in [[TANonEmptyList.reduceLeft]] and
 * [[TANonEmptyList.reduceRight]].
 *
 * Inspired by
 * [[https://github.com/TomasMikula/type-aligned/blob/7c57bad92d6b999273f6965d5a859e9509b6c822/src/main/scala/typealigned/APair.scala
 * Tomas Mikula's APair]].
 */
abstract class TATuple2K[F[_], G[_]] {
  type A

  def first: F[A]

  def second: G[A]
}

object TATuple2K {

  def apply[F[_], G[_], X](f: F[X], g: G[X]): TATuple2K[F, G] =
    new TATuple2K[F, G] {
      type A = X

      def first = f
      def second = g
    }
}
