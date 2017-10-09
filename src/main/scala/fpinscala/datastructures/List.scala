package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  /** Exercise 3.11 */
  def sum(ints: List[Int]): Int = foldLeft(0, ints)(_+_)

  /** Exercise 3.11 */
  def product(dubs: List[Double]): Double = foldLeft(1.0, dubs)(_*_)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /** Exercise 3.1 */
  def matchList(ints: List[Int]): Int = ints match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /** Exercise 3.2 */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => l
  }

  /** Exercise 3.3 */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => Cons(h, Nil)
  }

  /** Exercise 3.4 */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  /** Exercise 3.5 */
  @annotation.tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l
  }

  /** Exercise 3.14 */
  def append[A](a: List[A], b: List[A]): List[A] = {
    val accrue = (h: A, t: List[A]) => Cons(h, t)
    foldRight(a, b)(accrue)
  }

  /** Exercise 3.6 */
  def init[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n > 0 => Cons(h, init(t, n - 1))
    case _ => Nil
  }

  /** Exercise 3.9 */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b: Int) => b + 1)

  /** Exercise 3.12 */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(Nil: List[A], l)((b: List[A], a: A) => Cons(a, b))

  /** Exercise 3.15 */
  def accrete[A,B](l: List[List[A]]): List[A] = foldLeft(Nil: List[A], l)(append)

  /** Exercise 3.16 */
  def increment(l: List[Int]): List[Int] = l match {
    case Cons(h, t) => Cons(h + 1, increment(t))
    case _ => Nil
  }

  /** Exercise 3.17 */
  def stringify(l: List[Double]): List[String] = l match {
    case Cons(h, t) => Cons(h.toString, stringify(t))
    case _ => Nil
  }

  /** Exercise 3.18 */
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case _ => Nil
  }

  /** Exercise 3.19 */
  def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => Cons(h, filter(t)(p))
    case Cons(h, t) if !p(h) => filter(t)(p)
    case _ => Nil
  }

  /** Exercise 3.20 */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft(Nil: List[B], l)((b: List[B], a: A) => append(b, f(a)))

  /** Exercise 3.21 */
  def select[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  /** Exercise 3.22 */
  def zipWithAdd(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (Nil, _) => Nil
    case(_, Nil) => Nil
    case (Cons(lh, lt), Cons(mh, mt)) => Cons(lh + mh, zipWithAdd(lt, mt))
  }

  /** Exercise 3.23 */
  def zipWith[A,B,C](l: List[A], m: List[B])(f: (A,B) => C): List[C] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case(Cons(lh, lt), Cons(mh, mt)) => Cons(f(lh, mh), zipWith(lt, mt)(f))
  }

  /** Exercise 3.24 */



  /** Exercise 3.10 */
  @annotation.tailrec
  private def foldLeft[A,B](b: B, l: List[A])(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(f(b, h), t)(f)
    case Nil => b
  }

  /** Exercise 3.13 */
  private def foldRight[A,B](l: List[A], b: B)(f: (A, B) => B): B =
    foldLeft(b, reverse(l))((b, a) => f(a, b))

  /** Listing 3.2 */
  private def foldR[A,B](l: List[A], b: B)(f: (A, B) => B): B = l match {
    case Nil => b
    case Cons(h, t) => f(h, foldRight(t, b)(f))
  }
}
