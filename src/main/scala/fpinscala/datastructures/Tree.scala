package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /** Exercise 3.25 */
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  /** Exercise 3.26 */
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
  }

  /** Exercise 3.27 */
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + depth(left) max 1 + depth(right)
    case Leaf(_) => 0
  }

  /** Exercise 3.28 */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((a: A) => Leaf(f(a)), (b: Tree[B], c: Tree[B]) => Branch(b, c))

  /** Exercise 3.29 */
  def fold[A,B](t: Tree[A])(f: A => B, g: (B, B) => B): B = t match {
    case Branch(left, right) => g(fold(left)(f, g), fold(right)(f, g))
    case Leaf(value) => f(value)
  }
}
