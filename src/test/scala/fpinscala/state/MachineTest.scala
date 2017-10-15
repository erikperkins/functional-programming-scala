package fpinscala.state

import org.scalatest.FunSuite
import Machine._
import State.{ unit, modify}

/** Exercise 6.11 */
class MachineTest extends FunSuite {
  val machine = Machine(locked = true, candies = 100, coins = 0)

  test("coin locked machine") {
    val ((candies, coins), m) = operate(Coin).from(machine)

    assert(coins == 1)
    assert(candies == 100)
    assert(m == Machine(locked = false, candies = 100, coins = 1))
  }

  test("turn locked machine") {
    val ((candies, coins), m) = operate(Turn).from(machine)

    assert(coins == 0)
    assert(candies == 100)
    assert(m == Machine(locked = true, candies = 100, coins = 0))
  }

  test("operate machine with explicit states") {
    val (_, coined) = operate(Coin).from(machine)
    val (_, turned) = operate(Turn).from(coined)

    assert(turned == Machine(true, 99, 1))
  }

  test("operate machine with flatMap") {
    val (_, dispensed) = operate(Coin).flatMap(s => operate(Turn)).from(machine)

    assert(dispensed == Machine(true, 99, 1))
  }

  test("operate machine with for comprehension") {
    val operations = for {
      _ <- operate(Coin)
      m <- operate(Turn)
    } yield m

    val ((candies, coins), _) = operations.from(machine)
    val (_, state) = operations.from(machine)

    assert(candies == 99)
    assert(coins == 1)
    assert(state == Machine(true, 99, 1))
  }

  test("string in state monad") {
    val wrapper = for {
      m <- unit[Machine, String]("hello")
      _ <- modify[Machine](_ => Machine(false, 0, 0))
    } yield m

    val (contents, _) = wrapper.from(machine)
    val (_, state) = wrapper.from(machine)

    assert(contents == "hello")
    assert(state == Machine(false, 0, 0))
  }
}
