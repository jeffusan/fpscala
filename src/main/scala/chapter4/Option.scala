package chapter4

import Math.pow

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f


}

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = for {
    mean <- mean(xs)
    value <- Some(xs.fold(0.0){(x,y) => pow(y - mean, 2.0) + x })
  } yield value / xs.size

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case opt :: opts => opt.flatMap{ x =>  // any time opt is None, this will short circuit to None
      sequence(opts).map{ (xs: List[A]) =>
        x :: xs
      }
    }
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f))((b: B, bs: List[B]) => b :: bs)
  }

  def sequenceInTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)  
}
