package fpinscala.handlingerrors

import scala.{ Either => _}

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[F>:E,B](f: A => Either[F,B]): Either[F,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[F>:E,B>:A](eb: =>  Either[F,B]): Either[F,B] = this match {
    case Left(_) => eb
    case Right(a) => Right(a)
  }

  def map2[F>:E,B,C](eb: Either[F,B])(f: (A,B) => C): Either[F,C] =
    flatMap(a => eb.map(b => f(a,b)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(n: Double, d: Double): Either[Exception, Double] = {
    try Right(n / d)
    catch { case e: Exception => Left(e) }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  /** Exercise 4.7 */
  def traverse[E,A,B](la: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    la.foldRight(Right(Nil): Either[E,List[B]])((a, elb) => f(a).map2(elb)(_::_))

  def sequence[E,A](lea: List[Either[E,A]]): Either[E,List[A]] =
    traverse(lea)(ea => ea)
}