package com.salesforce.maligned

import cats.arrow.{Category, Compose}
import cats.data.{Chain, NonEmptyChain}
import cats.evidence.Is
import cats.implicits._
import cats.{Eval, Monad}

import annotation.tailrec

/**
 * A type-aligned cons list.
 *
 * In a type-aligned cons list, the "output" (or right) type of a node is equivalent to the "input"
 * (or left) type of the node after it. For example, you could chain functions in a type aligned
 * list like `String => Option[Int], Option[Int] => Long, Long => Boolean`.
 *
 * @see [[TANonEmptyList]] for type-aligned lists that are guaranteed to not be empty.
 */
sealed abstract class TAList[F[_, _], A, B] extends Serializable {

  /**
   * This is similar to calling `toNel` on a `List[A]` to get a `NonEmptyList[A]`, but for
   * type-aligned lists. One difference is that instead of returning `None` if the list is empty,
   * this returns a `Left` with proof that the input and output types for the list are the same.
   * This proof is occasionally useful for convincing the compiler that your code is type-safe.
   */
  def toTANel: Either[A Is B, TANonEmptyList[F, A, B]]

  /**
   * Return a new list with `f` prepended to this list.
   */
  def ::[C](f: F[C, A]): TANonEmptyList[F, C, B]

  /**
   * Return a new list with the entire `init` list prepended to this list.
   */
  def :::[C](init: TAList[F, C, A]): TAList[F, C, B]

  /**
   * Perform a type-aligned left-fold over this list, accumulating into a `G[I, B]` result.
   *
   * @see [[TAFoldLeft]] for more details and a description of the type parameters.
   */
  def foldLeft[G[_, _], I](fold: TAFoldLeft[F, G, A, I]): G[I, B]

  /**
   * Perform a type-aligned right-fold over this list, accumulating into a `G[A, O]` result.
   *
   * @see [[TAFoldRight]] for more details and a description of the type parameters.
   */
  def foldRight[G[_, _], O](fold: TAFoldRight[F, G, B, O]): Eval[G[A, O]]

  def reverse: TAList.Rev[F, A, B]

  /**
   * Return a new list with all `F` elements transformed into `G` elements.
   */
  def mapK[G[_, _]](f: FunctionK2[F, G]): TAList[G, A, B]

  /**
   *  Use `F`'s `Category` instance to compose all elements into a single `F[A, B]`.
   */
  def composeAll(implicit F: Category[F]): F[A, B]

  def toChain: Chain[F[_, _]] = {
    val fold = new TAFoldLeft[F, λ[(a, b) => Chain[F[_, _]]], A, A] {
      override def init: Chain[F[_, _]] = Chain.empty

      override def step[P, R](acc: Chain[F[_, _]], f: F[P, R]): Chain[F[_, _]] =
        acc.append(f)

    }
    foldLeft[λ[(a, b) => Chain[F[_, _]]], A](fold)
  }

  def toList: List[F[_, _]] = toChain.toList
}

object TAList {

  /**
   * A reversed type-aligned list. Consider a `TAList[Function1, A, D]` with elements `A => B, B =>
   * C, C => D`.  If you reverse the list, the elements become `C => D, B => C, A => B`. Note that
   * this is ''not'' of type `TAList[Function1, D, A]`. It cannot be chained into a `D => A`,
   * because the first element is a function that takes a `C` (not a `D`), and the next element
   * takes an input type of `B` (which is neither `C` nor `D`). In general the right-type of each
   * element does not line up with the left-type of the next element. However, if you were to define
   * `type <=[A, B] = B => A` then you could view this list as `D <= C, C <= B, B <= A`. This ''is''
   * of the correct shape for a type-aligned list, but `F` is no longer `Funtion1`/`=>`. It is the
   * flipped `<=` type.
   *
   * This reversed list is a little awkward but can still be useful in certain situations. If you
   * wanted to collapse a `TAList[Function1, A, D]` into a single `A => D` you could do it with a
   * `foldLeft` that calls `andThen` to accumulate all of the function values together. If instead
   * you had a `TAList.Rev[Function1, A, D]` you could perform an equivalent `leftFold` into an `A
   * => D` by calling `compose` instead of `andThen`.
   *
   * This reversed shape can also be useful for the implementation of operations like
   * [[TANonEmptyList.reduceRight]].
   */
  type Rev[F[_, _], A, B] = TAList[Flip[F, *, *], B, A]

  def nil[F[_, _], A]: TAList[F, A, A] = TANil()

  implicit def categoryTAList[F[_, _]]: Category[TAList[F, *, *]] =
    new Category[TAList[F, *, *]] {

      override def compose[A, B, C](f: TAList[F, B, C], g: TAList[F, A, B]): TAList[F, A, C] =
        g ::: f
      override def id[A]: TAList[F, A, A] = nil
    }
}

final case class TANil[F[_, _], A]() extends TAList[F, A, A] {

  override def toTANel: Either[A Is A, TANonEmptyList[F, A, A]] = Left(Is.refl)

  override def ::[C](f: F[C, A]): TANonEmptyList[F, C, A] = TAOne(f)

  override def :::[C](init: TAList[F, C, A]): TAList[F, C, A] = init

  override def foldLeft[G[_, _], I](fold: TAFoldLeft[F, G, A, I]): G[I, A] = fold.init

  override def foldRight[G[_, _], O](fold: TAFoldRight[F, G, A, O]): Eval[G[A, O]] = fold.init

  override def reverse: TAList.Rev[F, A, A] = TAList.nil[Flip[F, *, *], A]

  override def composeAll(implicit F: Category[F]): F[A, A] = F.id

  override def mapK[G[_, _]](f: FunctionK2[F, G]): TAList[G, A, A] = TANil()
}

sealed abstract class TANonEmptyList[F[_, _], A, B] extends TAList[F, A, B] { self =>

  /**
   * If this is a single-element list, return `Left` containing the element; otherwise return a
   * `Right` containing the head and the tail of the list.
   */
  def uncons1: Either[F[A, B], TACons[F, A, B]]

  /**
   * Perform a type-aligned left-reduce over this list, accumulating into a `G[I, B]` result.
   *
   * @see [[TAReduceLeft]] for more details and a description of the type parameters.
   */
  def reduceLeft[G[_, _], I](r: TAReduceLeft[F, G, A, I]): G[I, B]

  override def foldLeft[G[_, _], I](fold: TAFoldLeft[F, G, A, I]): G[I, B] =
    reduceLeft(fold.toReduce)

  override def foldRight[G[_, _], O](fold: TAFoldRight[F, G, B, O]): Eval[G[A, O]] =
    reduceRight(fold.toReduce)

  override def toTANel: Either[A Is B, TANonEmptyList[F, A, B]] = Right(this)

  /**
   * Perform a type-aligned right-reduce over this list, accumulating into a `G[A, O]` result.
   *
   * @see [[TAReduceRight]] for more details and a description of the type parameters.
   */
  def reduceRight[G[_, _], O](r: TAReduceRight[F, G, B, O]): Eval[G[A, O]] = {
    // Note: reduceRight could be implemented simply by recursively calling `reduceRight` on the
    // tail of the list, but doing so wouldn't be tail-recursive and could result in a stack
    // overflow for large lists.
    @inline def tuple[X](rem: TANonEmptyList.Rev[F, A, X], acc: Eval[G[X, O]]) =
      TATuple2K[TANonEmptyList.Rev[F, A, *], λ[x => Eval[G[x, O]]], X](rem, acc)

    @tailrec def go(
      t: TATuple2K[TANonEmptyList.Rev[F, A, ?], λ[x => Eval[G[x, O]]]]): Eval[G[A, O]] =
      t.first.uncons1 match {
        case Left(a) => r.step(a, t.second)
        case Right(ht) => go(tuple(ht.tail, r.step(ht.head, t.second)))
      }

    reverse.uncons1 match {
      case Left(a) => r.init(a)
      case Right(ht) => go(tuple(ht.tail, r.init(ht.head)))
    }
  }

  override def :::[I](init: TAList[F, I, A]): TANonEmptyList[F, I, B] = {
    val fold = new TAFoldRight[F, TANonEmptyList[F, *, *], A, B] {
      override def init: Eval[TANonEmptyList[F, A, B]] = Eval.now(self)
      override def step[L, P](
        f: F[L, P],
        g: Eval[TANonEmptyList[F, P, B]]): Eval[TANonEmptyList[F, L, B]] =
        g.map(f :: _)

    }
    init.foldRight[TANonEmptyList[F, *, *], B](fold).value
  }

  override def composeAll(implicit F: Category[F]): F[A, B] = composeAllNonEmpty

  override def mapK[G[_, _]](f: FunctionK2[F, G]): TANonEmptyList[G, A, B] = {
    val reducer = new TAReduceRight[F, λ[(a, b) => TANonEmptyList[G, a, b]], B, B] {
      override def init[C](x: F[C, B]): Eval[TANonEmptyList[G, C, B]] =
        Eval.now(TANonEmptyList.one(f(x)))

      override def step[L, P](
        x: F[L, P],
        g: Eval[TANonEmptyList[G, P, B]]): Eval[TANonEmptyList[G, L, B]] =
        g.map(l => f(x) :: l)

    }
    reduceRight(reducer).value
  }

  override def ::[C](f: F[C, A]): TACons[F, C, B] = TACons(f, this)

  override def reverse: TANonEmptyList.Rev[F, A, B] = {
    val reducer = new TAReduceLeft[F, TANonEmptyList.Rev[F, *, *], A, A] {
      override def init[P](f: F[A, P]): TANonEmptyList.Rev[F, A, P] = TANonEmptyList.revOne(f)
      override def step[P, C](
        acc: TANonEmptyList.Rev[F, A, P],
        f: F[P, C]): TANonEmptyList.Rev[F, A, C] =
        f :: acc
    }

    reduceLeft[TANonEmptyList.Rev[F, *, *], A](reducer)
  }

  /**
   * Similar to [[composeAll]] but for non-empty lists. For non-empty lists, the constraint on `F`
   * relaxes from `Category` to `Compose`, because the lack of empty-list means that an identity for
   * `F` isn't needed.
   */
  def composeAllNonEmpty(implicit F: Compose[F]): F[A, B] = {
    val reducer = new TAReduceLeft[F, F, A, A] {
      override def init[P](f: F[A, P]): F[A, P] = f

      override def step[P, C](acc: F[A, P], f: F[P, C]): F[A, C] = F.compose(f, acc)

    }
    reduceLeft(reducer)
  }

  /**
   * A non-empty version of [[toChain]].
   */
  def toNonEmptyChain: NonEmptyChain[F[_, _]] = {
    val reducer = new TAReduceLeft[F, λ[(a, b) => NonEmptyChain[F[_, _]]], A, A] {

      override def init[P](f: F[A, P]): NonEmptyChain[F[_, _]] = NonEmptyChain.one(f)

      override def step[P, C](acc: NonEmptyChain[F[_, _]], f: F[P, C]): NonEmptyChain[F[_, _]] =
        acc.append(f)

    }
    reduceLeft[λ[(a, b) => NonEmptyChain[F[_, _]]], A](reducer)
  }
}

object TANonEmptyList {

  /**
   * @see [[TAList.Rev]]. This version is similar but guaranteed to be non-empty.
   */
  type Rev[F[_, _], A, B] = TANonEmptyList[Flip[F, *, *], B, A]

  def one[F[_, _], A, B](f: F[A, B]): TANonEmptyList[F, A, B] = TAOne(f)

  def revOne[F[_, _], A, B](f: F[A, B]): Rev[F, A, B] =
    one[Flip[F, *, *], B, A](f)

  implicit def composeTANonEmptyList[F[_, _]]: Compose[TANonEmptyList[F, *, *]] =
    new Compose[TANonEmptyList[F, *, *]] {

      override def compose[A, B, C](
        f: TANonEmptyList[F, B, C],
        g: TANonEmptyList[F, A, B]): TANonEmptyList[F, A, C] = g ::: f
    }

  /**
   * Create a [[TANonEmptyList]] instance by providing the right-most element and a function that
   * can be repeatedly be called to produce the element that should be prepended to the list.
   *
   * @see [[TAUnfoldRightNonEmpty]] for a more detailed explanation and a description of the type
   * parameters.
   */
  def unfoldM[F[_, _], S[_], M[_], L, R](unfold: TAUnfoldRightNonEmpty[F, S, M, L, R])(implicit
    M: Monad[M]): M[TANonEmptyList[F, L, R]] = {

    // initialize the state and accumulation list
    val unfoldInit = unfold.init
    val init: TATuple2K[S, TANonEmptyList[F, *, R]] =
      TATuple2K[S, TANonEmptyList[F, *, R], unfoldInit.A](
        // the initial state
        unfoldInit.first,
        // The initial accumulation list is a single element list of just the right-most element. We
        // will prepend to this accumulation list to add elements.
        TANonEmptyList.one(unfoldInit.second)
      )

    // repeatedly call `prev` on the provided unfold and update the state and the accumulation list
    // (by prepending an element to it). Do this until `prev` returns a `Left`, indicating that this
    // is the final element to prepend.
    //
    // Note: in Cats, `Monad.tailRecM` uses a `Right` to indicate termination and a `Left` to
    // indicate continuation. This seemed backwards to Cody, so he made `TAUnfoldRightNonEmpty` use
    // a `Left` to indicate termination and a `Right` to indicate continuation. This chunk of code
    // is where the two collide, so he apologizes for the confusion about left vs. right and stop
    // vs. go.
    M.tailRecM(init) { t =>
      val currentState = t.first
      val currentList = t.second
      // current head of the accumulation list
      val head: F[t.A, _] = currentList.uncons1.fold(identity, _.head)
      unfold
        .prev(currentState, head)
        .map(
          _.fold(
            // this is the last element to prepend to the result list
            terminator => (terminator :: currentList).asRight,
            // There are still more elements to prepend. We need to update the state and prepend an
            // element and then continue the `prev` calls.
            { stateAndPrev =>
              val updatedState = stateAndPrev.first
              val prevElement = stateAndPrev.second

              (TATuple2K[S, TANonEmptyList[F, *, R], stateAndPrev.A](
                updatedState,
                prevElement :: currentList)).asLeft
            }
          ))
    }
  }
}

/**
 * A single-element list wrapping an `F[A, B]`.
 */
final case class TAOne[F[_, _], A, B](value: F[A, B]) extends TANonEmptyList[F, A, B] {
  def uncons1: Either[F[A, B], TACons[F, A, B]] = Left(value)
  def reduceLeft[G[_, _], I](r: TAReduceLeft[F, G, A, I]): G[I, B] = r.init(value)
  override def reduceRight[G[_, _], O](r: TAReduceRight[F, G, B, O]): Eval[G[A, O]] = r.init(value)
}

/**
 * A type-aligned list of length 2 or more composed of a head element and a non-empty tail list.
 */
sealed abstract class TACons[F[_, _], A, B] extends TANonEmptyList[F, A, B] {

  /**
   * The "pivot" type between the head and tail of this list. This is the output type of the head
   * element in the list and the input type of the second element in the list.
   *
   * This might be referred to as an "existential" type for `TACons` because it's some type that
   * exists, but it's not a "known" type that is represented in the type `TACons[F, A, B]`.
   */
  type P

  def head: F[A, P]

  def tail: TANonEmptyList[F, P, B]

  def uncons1: Either[F[A, B], TACons[F, A, B]] = Right(this)

  override def reduceLeft[G[_, _], I](r: TAReduceLeft[F, G, A, I]): G[I, B] = {
    // Note: reduceLeft could be implemented simply by recursively calling `reduceLeft` on the
    // tail of the list, but doing so wouldn't be tail-recursive and could result in a stack
    // overflow for large lists.
    @inline def tuple[X](acc: G[I, X], rem: TANonEmptyList[F, X, B]) =
      TATuple2K[G[I, *], TANonEmptyList[F, *, B], X](acc, rem)

    @tailrec def go(t: TATuple2K[G[I, *], TANonEmptyList[F, *, B]]): G[I, B] =
      t.second.uncons1 match {
        case Left(f) => r.step(t.first, f)
        case Right(cons) => go(tuple(r.step(t.first, cons.head), cons.tail))
      }
    go(tuple(r.init(head), tail))
  }
}

object TACons {

  def apply[F[_, _], A, X, B](h: F[A, X], t: TANonEmptyList[F, X, B]): TACons[F, A, B] =
    new TACons[F, A, B] {
      type P = X
      def head: F[A, P] = h
      def tail: TANonEmptyList[F, P, B] = t
    }
}
