package com.salesforce.maligned

import cats.arrow.{Category, Compose}
import cats.data.Kleisli
import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.{CategoryTests, ComposeTests, MiniInt}
import cats.{Eq, Eval}
import org.typelevel.discipline.scalatest.Discipline

import TAListGen._

@SuppressWarnings(Array("org.wartremover.warts.ToString")) // whatever these are just tests
class TAListSpec extends MalignedSpec with Discipline {
  import TAListSpec._

  test("compose nel") {
    forAll { (x: Int) =>
      val l = intToDouble compose TANonEmptyList.one((_: Int) + 1)
      l.composeAllNonEmpty.apply(x) should ===((x + 1).toDouble)
    }
  }

  test("compose list") {
    forAll { (x: Int) =>
      val l: TAList[Function1, Int, Double] = intToDouble compose TANonEmptyList.one((_: Int) + 1)
      l.composeAll.apply(x) should ===((x + 1).toDouble)
    }
  }

  test("composeAllNonEmpty") {
    forAll { (x: Int) =>
      intToDouble.composeAllNonEmpty.apply(x) should ===(x.toDouble)
    }
  }

  test("composeAllnonEmpty consistent with composeAll") {
    forAll { (x: Int, l: TANonEmptyList[Function1, Int, Double]) =>
      l.composeAllNonEmpty.apply(x) should ===(l.composeAll.apply(x))
    }
  }

  test("reverse and foldRight consistent with foldLeft") {
    val foldL = new TAFoldLeft[Function1, Function1, Int, Int] {
      override def init: Int => Int = identity
      override def step[P, R](acc: Int => P, f: P => R): Int => R = f compose acc
    }

    val foldR = new TAFoldRight[Flip[Function1, *, *], Flip[Function1, *, *], Int, Int] {
      override def init: Eval[Int => Int] = Eval.now(identity)

      override def step[L, P](f: P => L, g: Eval[Int => P]): Eval[Int => L] = g.map(f compose _)
    }

    forAll { (x: Int, l: TAList[Function1, Int, Int]) =>
      val fl = l.foldLeft(foldL)
      val fr = l.reverse.foldRight[Flip[Function1, *, *], Int](foldR).value
      fl(x) should ===(fr(x))
    }
  }

  test("toTANel nil") {
    TAList.nil[Function1, Int].toTANel.isLeft should ===(true)
  }

  test("reverse reverse nil") {
    TAList.nil[Function1, Int].reverse.toTANel.isLeft should ===(true)
  }

  test("mapK nil") {
    TAList.nil[Function1, Int].mapK(wrapSome).toTANel.isLeft should ===(true)
  }

  test("one and x :: nil equivalence") {
    forAll { (i: Int, f: Int => Long) =>
      val x = (f :: TAList.nil).composeAllNonEmpty
      val y = TANonEmptyList.one(f).composeAllNonEmpty
      x(i) should ===(y(i))
    }
  }

  test("mapK identity") {
    forAll { (x: Int, l: TAList[Function1, Int, Int]) =>
      l.mapK(FunctionK2.identity).composeAll.apply(x) should ===(l.composeAll.apply(x))
    }
  }

  test("mapK Some") {
    forAll { (x: Int, l: TAList[Function1, Int, Int]) =>
      l.mapK(wrapSome).composeAll.apply(x) should ===(Some(l.composeAll.apply(x)))
    }
  }

  test("double reverse is a noop") {
    forAll { (x: Int, l: TAList[Function1, Int, Int]) =>
      l.reverse.reverse.composeAll.apply(x) should ===(l.composeAll.apply(x))
    }
  }

  test(":::") {
    forAll { (b: Byte) =>
      val l = TANonEmptyList.one((_: Byte).toInt) ::: intToDouble
      l.composeAllNonEmpty.apply(b) should ===(b.toDouble)
    }
  }

  test("toNonEmptyChain") {
    val l = 1.0.asLeft[String] :: TANonEmptyList.one(2.asRight[String])
    l.toNonEmptyChain.map(_.toString).mkString_(",") should ===("Left(1.0),Right(2)")
  }

  test("toChain and toNonEmptyChain are consistent") {
    val l = 1.0.asLeft[String] :: TANonEmptyList.one(2.asRight[String])
    val nelS = l.toNonEmptyChain.map(_.toString)
    val lS = l.toChain.map(_.toString)
    nelS.toChain should ===(lS)
  }

  test("toList and toChain are consistent") {
    val l = 1.0.asLeft[String] :: TANonEmptyList.one(2.asRight[String])
    l.toList.map(_.toString) should ===(l.toChain.map(_.toString).toList)
  }

  checkAll(
    "TAList[Function1, *, *]",
    CategoryTests[TAList[Function1, *, *]].category[MiniInt, MiniInt, MiniInt, MiniInt])

  checkAll(
    "TANonEmptyList[Function1, *, *]",
    ComposeTests[TANonEmptyList[Function1, *, *]].compose[MiniInt, MiniInt, MiniInt, Long])
}

object TAListSpec {

  val intToDouble: TANonEmptyList[Function1, Int, Double] =
    ((_: Int).toLong) ::
      ((x: Long) => x) ::
      TANonEmptyList.one((_: Long).toDouble)

  val wrapSome: FunctionK2[Function1, Kleisli[Option, *, *]] =
    new FunctionK2[Function1, Kleisli[Option, *, *]] {
      def apply[A, B](f: A => B): Kleisli[Option, A, B] = Kleisli(f andThen (_.some))
    }

  /**
   * Questionable Eq instance for type-aligned lists that should probably only exist in tests.
   */
  implicit def eqTAList[F[_, _], A, B](implicit
    F: Category[F],
    eqFAB: Eq[F[A, B]]): Eq[TAList[F, A, B]] =
    Eq.instance((l1, l2) =>
      (l1.toChain.size === l2.toChain.size) && l1.composeAll === l2.composeAll)

  /**
   * Questionable Eq instance for non-empty type-aligned lists that should probably only exist in
   * tests.
   */
  implicit def eqTANonEmptyList[F[_, _], A, B](implicit
    F: Compose[F],
    eqFAB: Eq[F[A, B]]): Eq[TANonEmptyList[F, A, B]] =
    Eq.instance((l1, l2) =>
      (l1.toChain.size === l2.toChain.size) && l1.composeAllNonEmpty === l2.composeAllNonEmpty)
}
