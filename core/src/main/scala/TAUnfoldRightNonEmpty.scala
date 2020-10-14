/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

/**
 * A type-aligned, non-empty right-unfold. This provides a way to build up a type-aligned sequence
 * by starting with the right-most element ([[TAUnfoldRightNonEmpty.init]]) and prepending elements (via
 * [[TAUnfoldRightNonEmpty.prev]]).
 *
 * @tparam F The type of elements in the type-aligned sequence (ex: `Function1`).
 *
 * @tparam S State that can be used to determine the element to prepend. It is type-aligned with the
 * input type of the following element so that you can depend on certain properties of the following
 * element. For example, you could set `S` equal to `org.scalacheck.Gen` to ensure that the input
 * type of each element in your list has a `Gen` instance that you can use.
 *
 * @tparam M an effect type (generally monadic) required to generate each element. For example, you
 * could set `M` to `cats.effect.IO` if computing the element to prepend required a database lookup.
 *
 * @tparam L the input type in the overall structure
 *
 * @tparam R the output type of the overall structure
 */
trait TAUnfoldRightNonEmpty[F[_, _], S[_], M[_], L, R] {

  /**
   * The right-most element in the structure, along with state (`S`) that will later be used by
   * [[prev]] to compute the element to prepend to it.
   */
  def init: TATuple2K[S, F[*, R]]

  /**
   * Compute the element to prepend to the current element.
   *
   * @param s the current state
   *
   * @param f the element to be prepended to
   *
   * @return A `Left` with the overall input type (`L`) if prepending this element completes the
   * structure. Otherwise a `Right` with the new state and the element to prepend (in this case the
   * structure will continue to be built).
   */
  def prev[X, Y](s: S[X], f: F[X, Y]): M[Either[F[L, X], TATuple2K[S, F[*, X]]]]
}
