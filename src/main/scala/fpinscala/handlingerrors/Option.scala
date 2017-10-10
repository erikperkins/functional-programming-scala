package fpinscala.handlingerrors

import scala.{Option => _, Some => _}
import scala.math.pow

/** Exercise 4.1 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if (this.map(f).getOrElse(false)) this else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /** Exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).map(m => xs.map(x => math.pow(m - x, 2))).flatMap(vs => mean(vs))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /** Exercise 4.3 */
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap(a => ob.map(b => f(a,b))) // in Haskell, flatMap ~ >>=, map ~ >>

  def mapII[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for { a <- oa; b <- ob } yield f(a,b) // in Haskell, this is a do statement

  /** Exercise 4.4 */
  def sequence[A](loa: List[Option[A]]): Option[List[A]] = traverse(loa)(a => a)
    // = loa.foldRight(Some(Nil): Option[List[A]])((ob, oa) => map2(oa,ob)((a,b) => b::a))

  /** Exercise 4.5 */
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldRight(Some(Nil): Option[List[B]])((a, ob) => map2(f(a), ob)(_::_))
  }

}
