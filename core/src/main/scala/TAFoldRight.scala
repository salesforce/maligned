package com.salesforce.maligned

import cats.Eval

/**
 * A type-aligned foldRight.
 *
 * `Eval` is used as it is in `cats.Foldable.foldRight` to allow for lazy evaluation of the tail of
 * the list and potential termination without traversing the entire structure.
 *
 * @tparam F the type of each element in the input structure. For example `Function1`.
 *
 * @tparam G the type to accumulate into. It has type parameters to support maintaining type-aligned
 * properties in the accumulated object.
 *
 * @tparam Z the output type of the last element in the structure. Your [[TAFoldRight.init]] method
 * will need to have an input type that lines up with the output type of the last element of the
 * list, since the fold associates the last element with the initial accumulated result and then
 * associates working back from there.
 *
 * @tparam O the output type of the final accumulated structure. Your [[TAFoldRight.init]] method will
 * define what this needs to be.
 *
 * @see [[TAList.foldRight]] for a foldRight across a [[TAList]] using an instance of this
 *
 * @see [[TAFoldLeft]] for a foldLeft counterpart
 *
 * @see [[TAReduceRight]] for a reduceRight (non-empty structure) counterpart
 */
trait TAFoldRight[F[_, _], G[_, _], Z, O] { self =>

  /**
   * Produce the initial accumulation value. Its input type must line up with the output type of
   * the last element of the list, since the fold associates the last element with the initial
   * accumulated result and then associates working back from there.
   *
   * @return the initial accumulation value
   */
  def init: Eval[G[Z, O]]

  /**
   * Update the accumulated result, given the previous element from the structure.
   *
   * @param acc the current accumulated value
   * @param f the previous element in the structure
   * @return an updated accumulated value
   */
  def step[L, P](f: F[L, P], g: Eval[G[P, O]]): Eval[G[L, O]]

  def toReduce: TAReduceRight[F, G, Z, O] =
    new TAReduceRight[F, G, Z, O] {
      override def init[P](f: F[P, Z]): Eval[G[P, O]] = self.step(f, self.init)

      override def step[L, P](f: F[L, P], g: Eval[G[P, O]]): Eval[G[L, O]] = self.step(f, g)
    }
}
