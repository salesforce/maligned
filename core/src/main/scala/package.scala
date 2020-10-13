package com.salesforce

package object maligned {

  /**
   * A flipped binary type constructor. For example, `Flip[Function1, A, B]` would be `Function1[B,
   * A]`.
   */
  type Flip[F[_, _], A, B] = F[B, A]
}
