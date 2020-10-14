/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

/**
 * A function from `F[A, B]` to `G[A, B]` that is universally defined for all possible types `A` and
 * `B`.
 *
 * Like [[https://typelevel.org/cats/datatypes/functionk.html cats.arrow.FunctionK]], but for types
 * with shape `F[_, _]` instead of `F[_]`.
 */
trait FunctionK2[F[_, _], G[_, _]] {
  def apply[A, B](f: F[A, B]): G[A, B]
}

object FunctionK2 {

  def identity[F[_, _]]: FunctionK2[F, F] =
    new FunctionK2[F, F] {
      override def apply[A, B](f: F[A, B]): F[A, B] = f
    }
}
