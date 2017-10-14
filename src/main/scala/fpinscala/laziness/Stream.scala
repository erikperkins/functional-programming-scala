package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {
  /** Exercise 5.1 */
  def toList: List[A] = {
    @annotation.tailrec
    def collect(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => collect(t(), h()::l)
      case _ => l
    }

    collect(this, List()).reverse
  }

  /** Exercise 5.2 */
  def take(n: Int): List[A] = {
    @annotation.tailrec
    def collect(s: Stream[A], l: List[A], n: Int): List[A] = s match {
      case Cons(h, t) if n > 0 => collect(t(), h()::l, n - 1)
      case _ => l
    }

    collect(this, List(), n).reverse
  }

  /** Exercise 5.2 */
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def neglect(s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => neglect(t(), n - 1)
      case _ => s
    }

    neglect(this, n)
  }

  /** Exercise 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](b: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
    case _ => b
  }

  /** Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /** Exercise 5.5 */
  def _takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  /** Exercise 5.6 */
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a).orElse(b))

  /** Exercise 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])( (a, b) => cons(f(a), b))

  /** Exercise 5.7 */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  /** Exercise 5.7 */
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  /** Exercise 5.7 */
  def flatMap[B](f: A => Stream[B]) =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  /** Exercise 5.13 */
  def _map[B](f: A => B): Stream[B] = unfold(this)({
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  /** Exercise 5.13 */
  def _take(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
    case _ => None
  }

  /** Exercise 5.13 */
  def __takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  /** Exercise 5.13 */
  def zipWith[B,C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(lh, lt), Cons(rh, rt)) => Some(f(lh(), rh()), (lt(), rt()) )
    case _ => None
  }

  /** Exercise 5.13 */
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s) {
      case (Cons(lh, lt), Cons(rh, rt)) => Some(((Some(lh()), Some(rh())), (lt(), rt())))
      case (Cons(lh, lt), Empty) => Some((Some(lh()), None), (lt(), Empty))
      case (Empty, Cons(rh, rt)) => Some((None, Some(rh())), (Empty, rt()))
      case _ => None
    }

  /** Exercise 5.14 */
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).forAll {
    case (Some(r), Some(l)) => l == r
    case (Some(_), None) => true
    case _ => false
  }

  /** Exercise 5.15 */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some(cons(h(), t()), t())
    case _ => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))

  /** Exercise 5.16 */
  def scanRight[B](b: B)(f: (A,B) => B): Stream[B] = Empty
    // tails.map(_.foldRight(b)(f)) // Trouble with a lazy value

}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  /** Smart constructor - caches and annotates Cons */
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

    /** Smart constructor - annotates Empty */
  def empty[A]: Stream[A] = Empty

  /** Explicitly defining apply() forces the use of smart constructors */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** Exercise 5.8 */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /** Exercise 5.9 */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /** Exercise 5.10 */
  def fibs: Stream[Int] = {
    def fib(s: List[Int]): Stream[Int] = {
      val i = List(s(1), s(0) + s(1))
      cons(s(1), fib(i))
    }

    fib(List(0, 1))
  }

  /** Exercise 5.11 */
  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s) match {
    case None => empty
    case Some((h, t)) => cons(h, unfold(t)(f))
  }

  /** Exercise 5.12 */
  def _fibs: Stream[Int] =
    unfold(List(0, 1))(s => Some(s(1), List(s(1), s(0) + s(1))))

  /** Exercise 5.12 */
  def _from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  /** Exercise 5.12 */
  def _constant(n: Int): Stream[Int] = unfold(n)(a => Some((a, a)))

  /** Exercise 5.12 */
  def _ones: Stream[Int] = unfold(0)(_ => Some(1, 0))






}
