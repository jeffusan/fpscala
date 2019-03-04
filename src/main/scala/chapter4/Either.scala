package chapter4

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(b1 => f(a, b1)))

}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil       => Right(Nil)
      case x :: xs => x.flatMap(xx => sequence(xs).map(y => xx :: y))
    }
  }

  def traverse[E, A, B](es: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil       => Right(Nil)
      case x :: xs => f(x).flatMap(xx => traverse(xs)(f).map(y => xx :: y))
    }
  }

}
