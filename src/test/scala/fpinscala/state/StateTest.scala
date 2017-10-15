package fpinscala.state

import org.scalatest.FunSuite

class StateTest extends FunSuite {
  val state = State[String, Int]((0,_))

  /** Exercise 6.10 */
  test("unit") {
    val (n, s) = State.unit(1).from("")
    assert(n == 1)
    assert(s == "")
  }

  /** Exercise 6.10 */
  test("flatMap") {
    val (n, s) = state.flatMap(i => State.unit(i + 1)).from("")
    assert(n == 1)
    assert(s == "")
  }

  /** Exercise 6.10 */
  test("map") {
    val (n, s) = state.map(i => i + 1).from("")
    assert(n == 1)
    assert(s == "")
  }

  /** Exercise 6.10 */
  test("map2") {
    val condition = State[String, Boolean]((true,_))
    val (n, s) = state.map2(condition)((a,b) => s"$a: $b").from("")
    assert(n == "0: true")
    assert(s == "")
  }

  /** Exercise 6.10 */
  test("sequence") {
    val condition = State[String, Int]((1,_))
    val list = List(state, condition)
    val (l, s) = State.sequence(list).from("")
    assert(l == List(0, 1))
    assert(s == "")
  }

  test("get") {
    val (s, t) = State.get.from("start")
    assert(s == "start")
    assert(t == "start")
  }

  test("set") {
    val (s, t) = State.set("change").from("anything")
    assert(s === ())
    assert(t == "change")
  }

  test("modify") {
    val (s, t) = State.modify[String](_ + ", world").from("hello")
    assert(s === ())
    assert(t == "hello, world")
  }
}
