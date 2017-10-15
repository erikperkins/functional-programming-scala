package fpinscala.state

import State.{ get, modify }

/** Exercise 6.11 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def operate(input: Input): State[Machine, (Int,Int)] = for {
    _ <- modify[Machine](s => update(input, s))
    m <- get
  } yield (m.candies, m.coins)

  private def update(i: Input, m: Machine): Machine = (i, m) match {
    case (Coin, Machine(true, n, c)) => Machine(locked = false, n, c + 1)
    case (Turn, Machine(false, n, c)) => Machine(locked = true, n - 1, c)
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(true, _, _)) => m
    case(_, Machine(_, 0, _)) => m
  }
}
