package fpinscala.state

import org.scalatest.{ FunSuite, BeforeAndAfter }

class RNGTest extends FunSuite with BeforeAndAfter {
  val rng = SimpleRNG(42)

  test("nextInt") {
    val (n1, rng1) = rng.nextInt
    val (n2, _) = rng1.nextInt

    assert(n1 == 16159453)
    assert(n2 == -1281479697)
  }

  /** Exercise 6.1 */
  test("nonNegativeInt") {
    val (n1, rng1) = RNG.nonNegativeInt(rng)
    val (n2, _) = RNG.nonNegativeInt(rng1)

    assert(n1 == 16159453)
    assert(n2 == 1281479697)
  }

  /** Exercise 6.2 */
  test("double") {
    val (n, _) = RNG.double(rng)
    assert(n == 0.007524831689672932)
  }

  /** Exercise 6.3 */
  test("intDouble") {
    val (n, d) = RNG.intDouble(rng)
    assert(n == 16159453)
    assert(d == 0.5967354856416283)
  }

  /** Exercise 6.3 */
  test("doubleInt") {
    val (d, n) = RNG.doubleInt(rng)
    assert(d == 0.007524831689672932)
    assert(n == -1281479697)
  }

  /** Exercise 6.3 */
  test("tripleDouble") {
    val (d1, d2, d3) = RNG.tripleDouble(rng)

    assert(d1 == 0.007524831689672932)
    assert(d2 == 0.5967354856416283)
    assert(d3 == 0.15846728447753344)
  }

  /** Exercise 6.4 */
  test("int") {
    val (ints, _) = RNG.ints(3)(rng)
    assert(ints == List(-340305902, -1281479697, 16159453))
  }

  test("unit") {
    val (n, rng1) = RNG.unit(1)(rng)
    assert(n == 1)
    assert(rng1 == rng)
  }

  test("map") {
    val zero = RNG.unit(0)
    val (n, rng1) = RNG.map(zero)(i => i + 1)(rng)
    assert(n == 1)
    assert(rng1 == rng)
  }

  test("nonNegativeEven") {
    val (n, _) = RNG.nonNegativeEven(rng)
    assert(n == 16159452)
  }

  /** Exercise 6.5 */
  test("_double") {
    assert(RNG._double(rng) == 0.007524831689672932)
  }

  /** Exercise 6.6 */
  test("map2") {
    val one = RNG.unit(1)
    val two = RNG.unit(2)
    val (n, rng1) = RNG.map2(one, two)(_+_)(rng)
    assert(n == 3)
    assert(rng1 == rng)
  }

  test("both") {
    val one = RNG.unit(1)
    val a = RNG.unit("a")
    val ((n, c), rng1) = RNG.both(one, a)(rng)

    assert(n == 1)
    assert(c == "a")
    assert(rng1 == rng)
  }

  test("_intDouble") {
    val ((n, d), _) = RNG._intDouble(rng)
    assert(n == 16159453)
    assert(d == 0.5967354856416283)
  }

  test("_doubleInt") {
    val ((d, n), _) = RNG._doubleInt(rng)
    assert(d == 0.007524831689672932)
    assert(n == 1281479697)
  }

  /** Exercise 6.7 */
  test("sequence") {
    val (l, rng1) = RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)
    assert(l == List(1,2,3))
    assert(rng1 == rng)
  }

  /** Exercise 6.7 */
  test("_ints") {
    val (ints, rng1) = RNG._ints(3)(rng)
    assert(ints == List(16159453,1281479697,340305902))
  }

  /** Exercise 6.8 */
  test("flatMap") {
    val (one, rng1) = RNG.flatMap(RNG.unit(0))(i => RNG.unit(i + 1))(rng)
    assert(one == 1)
    assert(rng1 == rng)
  }

  /** Exercise 6.8 */
  test("nonNegativeLessThan") {
    val (less, _) = RNG.nonNegativeLessThan(16159453)(rng)
    assert(less < 16159453)
    assert(less == 12482336)
  }

  /** Exercise 6.9 */
  test("_map") {
    val zero = RNG.unit(0)
    val (n, rng1) = RNG._map(zero)(i => i + 1)(rng)
    assert(n == 1)
    assert(rng1 == rng)
  }

  /** Exercise 6.9 */
  test("_map2") {
    val one = RNG.unit(1)
    val two = RNG.unit(2)
    val (n, rng1) = RNG._map2(one, two)(_+_)(rng)
    assert(n == 3)
    assert(rng1 == rng)
  }

}
