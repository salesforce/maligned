/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */

package com.salesforce

package object maligned {

  /**
   * A flipped binary type constructor. For example, `Flip[Function1, A, B]` would be `Function1[B,
   * A]`.
   */
  type Flip[F[_, _], A, B] = F[B, A]
}
