/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

import cats.Eval

/**
 * A type-aligned reduceRight.
 *
 * `Eval` is used as it is in `cats.Reducible.reduceRight` to allow for lazy evaluation of the tail of
 * the list and potential termination without traversing the entire structure.
 *
 * @tparam F the type of each element in the input structure. For example `Function1`.
 *
 * @tparam G the type to accumulate into. It has type parameters to support maintaining type-aligned
 * properties in the accumulated object.
 *
 * @tparam Z the output type of the last element in the structure. Your [[TAFoldRight.init]] method
 * will need to take an element with this output type, since the fold associates the last element
 * with the initial accumulated result and then associates working back from there.
 *
 * @tparam O the output type of the final accumulated structure. Your [[TAFoldRight.init]] method will
 * define what this needs to be.
 *
 * @see [[TANonEmptyList.reduceRight]] for a reduceRight across a [[TANonEmptyList]] using an instance of
 * this
 *
 * @see [[TAReduceLeft]] for a reduceLeft counterpart
 *
 * @see [[TAFoldRight]] for a foldRight (potentially empty structure) counterpart
 */
trait TAReduceRight[F[_, _], G[_, _], Z, O] {

  /**
   * Produce the initial accumulation value given the last element of the structure. This must work
   * for any `P` type, because the input type of this will need to line up with the output type of
   * the previous element.
   *
   * @param f the last (right-most) element in the structure
   *
   * @return the initial accumulation value
   */
  def init[P](f: F[P, Z]): Eval[G[P, O]]

  /**
   * Update the accumulated result, given the previous element from the structure.
   *
   * @param acc the current accumulated value
   * @param f the previous element in the structure
   * @return an updated accumulated value
   */
  def step[L, P](f: F[L, P], g: Eval[G[P, O]]): Eval[G[L, O]]
}
