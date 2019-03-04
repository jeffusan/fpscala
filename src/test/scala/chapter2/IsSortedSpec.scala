package chapter2

import org.scalatest._
import org.scalatest.prop.PropertyChecks

trait Sorter {

  def intSorted(a: Int, b: Int): Boolean = a <= b

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      val b = as.toSeq.sliding(2) // get pairwise
      val c = b.map( pair => ordered(pair.head, pair.last)) // apply predicate
      c.foldLeft(true)(_ && _) // fold over results
  }

}

class IsSortedSpec extends PropSpec  with Sorter  with PropertyChecks  with Matchers {

  property("check for values of int") {
    forAll { a: Array[Int] =>
      (a.sortWith(intSorted).deep == a.deep) shouldBe isSorted(a, intSorted)
    }
  }
}
