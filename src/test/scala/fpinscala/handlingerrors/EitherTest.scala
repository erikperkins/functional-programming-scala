package fpinscala.handlingerrors

import org.scalatest.FunSuite

class EitherTest extends FunSuite {
  /** Exercise 4.6 */
  test("map") {
    val f: Int => Int = _ + 1

    assert(Right(0).map(f) == Right(1))
    assert(Left("error").map(f) == Left("error"))
  }

  test("flatMap") {
    val f = (a: Int) => if (a > 0) Right(a + 1) else Left("invalid")

    assert(Right(1).flatMap(f) == Right(2))
    assert(Right(0).flatMap(f) == Left("invalid"))
    assert(Left("error").flatMap(f) == Left("error"))
  }

  test("orElse") {
    val eb = Right(1)

    assert(Left("error").orElse(eb) == Right(1))
    assert(Right(0).orElse(eb) == Right(0))
  }

  test("map2") {
    val f = (a: Int, b: Int) => a + b

    assert(Right(1).map2(Right(2))(f) == Right(3))
    assert(Right(1).map2(Left("error"))(f) == Left("error"))
    assert(Left("error").map2(Right(2))(f) == Left("error"))
  }

  /** Exercise 4.8 */
  test("traverse") {
    val f = (a: Int) => if (a > 0) Right(a + 1) else Left("invalid")
    val l = List(1, 2, 3)
    val m = List(0, 1)

    assert(Either.traverse(l)(f) == Right(List(2, 3, 4)))
    assert(Either.traverse(m)(f) == Left("invalid"))
  }

  test("sequence") {
    val l = List(Right(1), Right(2))
    val m = List(Right(0), Left("first"), Left("second"), Right(1))

    assert(Either.sequence(l) == Right(List(1, 2)))
    assert(Either.sequence(m) == Left("first"))
  }
}
