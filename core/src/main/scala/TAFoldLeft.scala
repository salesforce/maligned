/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce.maligned

/**
 * A type-aligned foldLeft.
 *
 * @tparam F the type of each element in the input structure. For example `Function1`.
 *
 * @tparam G the type to accumulate into. It has type parameters to support maintaining type-aligned
 * properties in the accumulated object.
 *
 * @tparam Z the input type of the first element in the structure. Your [[TAFoldLeft.init]] method
 * will need to have an output type that lines up with the input type of the first element of the
 * list, since the fold starts from the left.
 *
 * @tparam I the input type of the final accumulated structure. Your [[TAFoldLeft.init]] method will
 * define what this needs to be.
 *
 * @see [[TAList.foldLeft]] for a foldLeft across a [[TAList]] using an instance of this
 *
 * @see [[TAFoldRight]] for a foldRight counterpart
 *
 * @see [[TAReduceLeft]] for a reduceLeft (non-empty structure) counterpart
 *
 * @see [[com.salesforceiq.intelligence.pipeline.Pipeline.toJava]] for an example usage.
 */
trait TAFoldLeft[F[_, _], G[_, _], Z, I] { self =>

  /**
   * The initial accumulation value. Since this will be consumed along with the the first element
   * in the structure, its output type needs to line up with the input type of the first element.
   */
  def init: G[I, Z]

  /**
   * Update the accumulated result, given the next element from the structure.
   *
   * @param acc the current accumulated value
   * @param f the next element in the structure
   * @return an updated accumulated value
   */
  def step[P, R](acc: G[I, P], f: F[P, R]): G[I, R]

  def toReduce: TAReduceLeft[F, G, Z, I] =
    new TAReduceLeft[F, G, Z, I] {
      override def init[P](f: F[Z, P]): G[I, P] = self.step(self.init, f)

      override def step[P, R](acc: G[I, P], f: F[P, R]): G[I, R] = self.step(acc, f)
    }
}
