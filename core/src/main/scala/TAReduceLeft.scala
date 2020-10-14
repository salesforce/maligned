/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

/**
 *  A type-aligned reduceLeft.
 *
 * @tparam F the type of each element in the input structure. For example `Function1`.
 *
 * @tparam G the type to accumulate into. It has type parameters to support maintaining type-aligned
 * properties in the accumulated object.
 *
 * @tparam Z the input type of the first element in the structure. Your [[TAReduceLeft.init]] method
 * will need to have an output type that lines up with the input type of the first element of the
 * list, since the fold starts from the left.
 *
 * @tparam I the input type of the final accumulated structure. Your [[TAReduceLeft.init]] method will
 * define what this needs to be.
 *
 * @see [[TANonEmptyList.reduceLeft]] for a reduceLeft across a [[TANonEmptyList]] using an instance of this
 *
 * @see [[TAReduceRight]] for a reduceRight counterpart
 *
 * @see [[TAFoldLeft]] for a foldLeft (potentially empty structure) counterpart
 */
trait TAReduceLeft[F[_, _], G[_, _], Z, I] {

  /**
   * Produce the initial accumulation value given the first element of the structure. This must
   * work for any `P` type, because the output of this will need to line up with the input type of
   * the following element.
   *
   * @param f the initial element in the structure
   *
   * @return the initial accumulation value
   */
  def init[P](f: F[Z, P]): G[I, P]

  /**
   * Update the accumulated result, given the next element from the structure.
   *
   * @param acc the current accumulated value
   * @param f the next element in the structure
   * @return an updated accumulated value
   */
  def step[P, R](acc: G[I, P], f: F[P, R]): G[I, R]
}
