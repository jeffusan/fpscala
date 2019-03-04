package chapter5

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class StreamSpec extends PropSpec with Matchers with PropertyChecks {

  val ten = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  property("Exercise 1 - Write a function to convert a Stream to a List") {
    ten.toList shouldBe (1 to 10).toList
  }

  property(
    "Exercise 2 - Write a function take for returning the first n elements of a Stream") {
    ten.take(3).toList shouldBe (1 to 3).toList
  }

  property(
    "Exercise 3 - Write the function takeWhile for returning all staring elements of a Stream that match a given predicate") {
    ten.takeWhile(_ <= 5).toList shouldBe (1 to 5).toList
  }

  property(
    "Exercise 4 - Implement forAll, which checks that all elements in the Stream match a given predicate.") {
    ten.forAll(_ <= 10) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ <= 3) shouldBe false
  }

  property("Exercise 5 - Use foldRight to implement takeWhile") {
    ten.takeWhile(_ <= 5).toList shouldBe (1 to 5).toList
  }

  property("Exercise 6 - Implement map, filter, append, and flatMap by using foldRight") {
    ten.map(_ * 2).toList shouldBe (1 to 10).map(_ * 2)

    ten.filter(_ <= 3).toList shouldBe (1 to 3)

    ten.append(Stream(11, 12, 13)).toList shouldBe (1 to 13)

    ten.flatMap((x: Int) => Stream(x + 1)).toList shouldBe (2 to 11)
  }
}
