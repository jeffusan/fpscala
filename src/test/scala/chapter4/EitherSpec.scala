package chapter4

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import chapter4._
import chapter4.Either._

class EitherSpec extends PropSpec with Matchers with PropertyChecks {

  property("Either.map") {
    Right(1) map((a) => a + 1) shouldBe Right(2)
    Left(1) map((a) => a) shouldBe Left(1)
  }

  property("Either.flatMap") {
    Right(1) flatMap((a) => Right(a + 1)) shouldBe Right(2)
    Left(1) flatMap((a) => Left(a)) shouldBe Left(1)
    Right(1) flatMap((a) => Left(a)) shouldBe Left(1)
  }

  property("Either orElse") {
    Right(1) orElse(Left(100)) shouldBe Right(1)
    Left(1) orElse(Left(100)) shouldBe Left(100)
  }

  property("Either.sequence") {
    sequence(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
    sequence(List(Right(1), Left(2))) shouldBe Left(2)
  }

  property("Either.traverse") {
    traverse(List(1, 2))(a => Right(a + 1)) shouldBe Right(List(2, 3))
    traverse(List(1, 2))(a => Left(1)) shouldBe Left(1)
}
}
