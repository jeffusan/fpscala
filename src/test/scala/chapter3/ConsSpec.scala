package chapter3

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class ConsSpec extends PropSpec with Matchers with PropertyChecks {

  val oneToTen = Gen.oneOf (
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10
  )

  val productDoubles = Gen.oneOf (
    (Cons(1.0, List(2.0)), 3.0),
    (Cons(1.0, List(2.0, 3.0)), 6.0)
    
  )

  val sumInts = Gen.oneOf (
    (Cons(1, List(2)), 3),
    (Cons(1, List(2, 3)), 6)
  )

  property("Create a list using Cons") {
    forAll(oneToTen) { i =>
      Cons(i, List())
      Cons(i, List(i))
    }
  }

  property("first product test") {
    forAll(productDoubles) { d =>
      List.product(d._1) == d._2
    }
  }

  property("first sum test") {
    forAll(sumInts) { i =>
      List.sum(i._1) == i._2
    }
  }

  property("second product test") {
    forAll(productDoubles) { d =>
      List.product2(d._1) == d._2
    }
  }

  property("second sum test") {
    forAll(sumInts) { i =>
      List.sum2(i._1) == i._2
    }
  }

  property("set head test") {
    val cons = Cons(1, List(1, 2))
    forAll(oneToTen) { i =>
      List.setHead(i, cons) match {
        case Cons(x, xs) => x == i
        case _ => fail("failed test set head")
      }
    }
  }

  property("dropwhile should drop some even ints") {
    // drop until test fails
    val i = Cons(2, List(4,6,8, 1))
    List.dropWhile(i)((a: Int) => a % 2 == 0) shouldBe Cons(1, List())
    val k = Cons(2, List(3,6,7))
    List.dropWhile(k)((a: Int) => a % 2 == 0) shouldBe Cons(3, List(6,7))
  }

  property("drop should drop by count") {
    val i = Cons(2, List(4,6,8, 1))
    List.drop(i, 1) shouldBe Cons(4, List(6,8,1))
    List.drop(i, 3) shouldBe Cons(8, List(1))
  }

  property("tailing") {
    val k = Cons(2, List(3,6,7))
    List.tail(k) shouldBe Cons(3, List(6,7))
  }

  property("appending") {
    val i = Cons(1, List(2,3))
    val j = Cons(4, List())
    List.append(i, j) shouldBe Cons(1, List(2,3,4))
  }

  property("initing") {
    val k = Cons(2, List(3,6,7))
    List.init(k) shouldBe Cons(2, List(3,6))
  }
}
