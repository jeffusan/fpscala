package chapter5

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class StreamSpec extends PropSpec with Matchers with PropertyChecks {

  val ten = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  property("Exercise 1 - convert a Stream to a List") {
    ten.toList shouldBe (1 to 10).toList
  }

  property(
    "Exercise 2 - take the first n elements of a Stream") {
    ten.take(3).toList shouldBe (1 to 3).toList
  }

  property(
    "Exercise 3 - takeWhile all elements of a Stream while match") {
    ten.takeWhile(_ <= 5).toList shouldBe (1 to 5).toList
  }

  property(
    "Exercise 4 - all elements in the Stream match a given predicate.") {
    ten.forAll(_ <= 10) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ <= 3) shouldBe false
  }

  property("Exercise 6 - Implement map, filter, append, and flatMap by using foldRight") {
    ten.map(_ * 2).toList shouldBe (1 to 10).map(_ * 2)

    ten.filter(_ <= 3).toList shouldBe (1 to 3)

    ten.append(Stream(11, 12, 13)).toList shouldBe (1 to 13)

    ten.flatMap((x: Int) => Stream(x + 1)).toList shouldBe (2 to 11)

    ten.headOption shouldBe Some(1)

    Stream().headOption shouldBe None
  }

  property("Exercise 8- return constant value repeatedly") {
    Stream.constant("aloha").take(3).toList shouldBe List("aloha","aloha","aloha")
  }

  property("Exercise 9- return an infinite range") {
    Stream.from(1).take(10).toList shouldBe (1 to 10)
  }

  property("Exercise 10 - generate fibs") {
    Stream.fibs().take(10).toList shouldBe List(0,1,1,2,3,5,8,13,21,34)
  }

  property("Exercise 16- scan right") {
    Stream(1,2,3).scanRight(0)(_ + _).toList shouldBe List(6,5,3,0)
  }
}
