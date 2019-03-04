package chapter5

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {

  import chapter5.Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def toList: List[A] = this match {
    case Cons(h, t) => h() +: t().toList
    case _          => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
    case _                    => Stream.empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop (n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()), None), (t1(), Stream.empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None, Some(h2())), (Stream.empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    unfold(this, s) {
      case (Empty, Cons(_, _))          => Some(false, (Stream.empty, Stream.empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(h1() == h2(), (t1(), t2()))
      case _                            => None
    }.forAll(a => a)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((Stream.cons(h(), t()), t()))
    }.append(Stream(Stream.empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream.empty[B])((h1, t1) => {
      t1 match {
        case Empty       => Stream(f(h1, z)).append(Stream(z))
        case Cons(h2, _) => Stream.cons(f(h1, h2()), t1)
      }
    })

}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def ones: Stream[Int] = unfold(1)(a => Some(a, a))

  def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))

  def fibs(): Stream[Int] = unfold((0, 1)) {
    case (a, b) => Some(a, (b, a + b))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}
