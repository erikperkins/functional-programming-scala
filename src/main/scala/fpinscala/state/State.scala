package fpinscala.state

import State._

case class State[S,+A](from: S => (A,S)) {
  /** Exercise 6.10 */
  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, s1) = this.from(s)
      g(a).from(s1)
    })

  /** Exercise 6.10 */
  def map[B](f: A => B): State[S,B] = flatMap(a => unit(f(a)))

  /** Exercise 6.10 */
  def map2[B,C](t: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => t.map(b => f(a,b)))
}

object State {
  /** Exercise 6.10 */
  def unit[S,A](a: A): State[S,A] = State(s => (a,s))

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  /** Exercise 6.10 */
  def sequence[S,A](l: List[State[S,A]]): State[S,List[A]] = {
    val end = unit[S, List[A]](Nil)
    l.foldRight(end)((sa, sl) => sa.map2(sl)(_::_))
  }
}