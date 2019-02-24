package chapter4

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import chapter4._
import chapter4.Option._

class OptionSpec extends PropSpec with Matchers with PropertyChecks {

  def failFunction(i: Int): Option[Int]= try {
    Some(10 / i)
  } catch { case e: Exception => None }
  
  property("test option map") {
    forAll { x: Int =>
      val actual = failFunction(x)
      if(x == 0) {
        actual shouldBe None
      } else {
        val expected = 10 / x
       actual shouldBe Some(expected)
      }

    }
  }

  property("test getorelse") {
    Some("Hello") getOrElse("World") shouldBe "Hello"
    None getOrElse("World") shouldBe "World"
  }

  property("test flatmap") {
    val addWorld = (a: String) => Some(s"$a World")
    Some("Hello") flatMap(addWorld) shouldBe Some("Hello World")
    None flatMap(addWorld) shouldBe None
  }

  property("test orElse") {
    Some("Hello") orElse(Some("World")) shouldBe Some("Hello")
    None orElse(Some("World")) shouldBe Some("World")
  }

  property("test filter") {
    val isOne = (a:Int) => a == 1
    Some(1) filter(isOne) shouldBe Some(1)
    None filter(isOne) shouldBe None
  }

  property("test variance") {
    variance(List(1,2,3,4,5)) shouldBe Some(2.0)
    variance(List()) shouldBe None
  }

  property("test sequence") {
    sequence(List(Some(1), Some(2))) shouldBe Some(List(1,2))
    sequence(List(None, Some(2))) shouldBe None
  }

  property("test traverse") {
    traverse(List(1,2))(a => Some(a)) shouldBe Some(List(1,2))
    traverse(List(1,2))(a => None) shouldBe None
  }
}
