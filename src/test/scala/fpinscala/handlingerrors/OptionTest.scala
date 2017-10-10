package fpinscala.handlingerrors

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  /** Exercise 4.1 */
  test("map") {
    val option = Some(0)
    val f: Int => Int = _ + 1

    assert(option.map(f) == Some(1))
    assert(None.map(f) == None)
  }

  /** Exercise 4.1 */
  test("getOrElse") {
    val option = Some(1)

    assert(option.getOrElse(0) == 1)
    assert(None.getOrElse(0) == 0)
  }

  /** Exercise 4.1 */
  test("flatMap") {
    val f = (a: Int) => if (a > 0) Some(a) else None

    assert(Some(1).flatMap(f) == Some(1))
    assert(Some(0).flatMap(f) == None)
    assert(None.flatMap(f) == None)
  }

  /** Exercise 4.1 */
  test("orElse") {
    assert(Some(0).orElse(Some(1)) == Some(0))
    assert(None.orElse(Some(1)) == Some(1))
  }

  /** Exercise 4.1 */
  test("filter") {
    val f: Int => Boolean = _ > 0

    assert(Some(1).filter(f) == Some(1))
    assert(Some(0).filter(f) == None)
    assert(None.filter(f) == None)
  }

  /** Exercise 4.2 */
  test("variance") {
    val xs = Seq(1.0, 0, -1.0)
    assert(Option.variance(xs) == Some(2.0/3.0))
  }

  test("map2") {
    val f: (Int, Int) => Int = _+_

    assert(Option.map2(Some(1), Some(2))(f) == Some(3))
  }

  /** Exercise 4.4 */
  test("sequence") {
    val s = List(Some(0), Some(1))
    val t = List(Some(0), None)

    assert(Option.sequence(s) == Some(List(0, 1)))
    assert(Option.sequence(t) == None)
  }

  /** Exercise 4.5 */
  test("traverse") {
    val l = List(1.2, 3.4)
    val m = List(0.1, 2.3)
    val f = (a: Double) => if (a > 1.0) Some(a.toString) else None

    assert(Option.traverse(l)(f) == Some(List("1.2", "3.4")))
    assert(Option.traverse(m)(f) == None)
  }
}
