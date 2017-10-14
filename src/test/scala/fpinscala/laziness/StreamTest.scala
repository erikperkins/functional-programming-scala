package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {
  /** Exercise 5.1 */
  test("toList") {
    val stream = Stream(1,2,3)
    assert(stream.toList == List(1,2,3))
  }

  /** Exercise 5.2 */
  test("take") {
    val stream = Stream(1,2,3)
    assert(stream.take(2) == List(1,2))
  }

  /** Exercise 5.2 */
  test("drop") {
    val stream = Stream(1,2,3)
    assert(stream.drop(1).toList == List(2,3))
  }

  /** Exercise 5.3 */
  test("takeWhile") {
    val stream = Stream(1,2,3)
    assert(stream.takeWhile(_ < 3).toList == List(1,2))
  }

  test("exists") {
    val stream = Stream(1,2,3)
    assert(stream.exists(_ == 1))
    assert(!stream.exists(_ == 4))
  }

  test("foldRight") {
    val stream = Stream("H", "e", "l", "l", "o")
    val f: (String, => String) => String = (a, b) => a + b
    assert(stream.foldRight(".")(f) == "Hello.")
  }

  /** Exercise 5.4 */
  test("forAll") {
    val stream = Stream(1,2,3)
    assert(stream.forAll(_ > 0))
    assert(!stream.forAll(_ < 0))
  }

  /** Exercise 5.5 */
  test("_takeWhile") {
    val stream = Stream(1,2,3)
    assert(stream.takeWhile(_ < 3).toList == List(1,2))
  }

  /** Exercise 5.6 */
  test("headOption") {
    val stream = Stream(1,2,3)
    assert(stream.headOption == Some(1))
    assert(Empty.headOption == None)
  }

  /** Exercise 5.7 */
  test("map") {
    val stream = Stream(1,2,3)
    assert(stream.map(_ + 1).toList == List(2,3,4))
  }

  /** Exercise 5.7 */
  test("filter") {
    val stream = Stream(1,2,3)
    assert(stream.filter(_ > 1).toList == List(2,3))
  }

  /** Exercise 5.7 */
  test("append") {
    val stream = Stream(1,2)
    assert(stream.append(Stream(3,4)).toList == List(1,2,3,4))
  }

  /** Exercise 5.7 */
  test("flatMap") {
    val stream = Stream(1,3,5)
    val f = (a: Int) => Stream(a, a + 1)
    assert(stream.flatMap(f).toList == List(1,2,3,4,5,6))
  }

  test("find") {
    assert(Stream(1,2).find(_ == 2) == Some(2))
    assert(Stream(1,2).find(_ == 3) == None)
  }

  /** Exercise 5.8 */
  test("constant") {
    val stream = Stream.constant(2)
    assert(stream.take(5) == List(2,2,2,2,2))
  }

  /** Exercise 5.9 */
  test("from") {
    val stream = Stream.from(1)
    assert(stream.take(3) == List(1,2,3))
  }

  /** Exercise 5.10 */
  test("fibs") {
    val stream = Stream.fibs
    assert(stream.take(6) == List(1,1,2,3,5,8))
  }

  /** Exercise 5.11 */
  test("unfold") {
    val f: Int => Option[(Int, Int)] =
      a => if (a > 10) None else Some((a, a * 3))
    assert(Stream.unfold(1)(f).toList == List(1,3,9))
  }

  /** Exercise 5.12 */
  test("_fibs") {
    assert(Stream._fibs.take(6) == List(1,1,2,3,5,8))
  }

  /** Exercise 5.12 */
  test("_from") {
    assert( Stream._from(1).take(3) == List(1,2,3))
  }

  /** Exercise 5.12 */
  test("_constant") {
    assert(Stream._constant(2).take(3) == List(2,2,2))
  }

  /** Exercise 5.12 */
  test("_ones") {
    assert(Stream._ones.take(3) == List(1,1,1))
  }

  /** Exercise 5.13 */
  test("_map") {
    val stream = Stream(1,2,3)
    assert(stream._map(_ + 1).toList == List(2,3,4))
  }

  /** Exercise 5.13 */
  test("_take") {
    val stream = Stream(1,2,3)
    assert(stream._take(2).toList == List(1,2))
  }

  /** Exercise 5.13 */
  test("__takeWhile") {
    val stream = Stream(1,2,3)
    assert(stream.__takeWhile(_ < 3).toList == List(1,2))
  }

  /** Exercise 5.13 */
  test("zipWith") {
    val left = Stream(1,2,3)
    val right = Stream(4,5,6)
    assert(left.zipWith(right)(_+_).toList == List(5,7,9))
  }

  /** Exercise 5.13 */
  test("zipAll") {
    val left = Stream(1,2,3)
    val right = Stream(1,2)
    val zipped = List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
    assert(left.zipAll(right).toList == zipped)
  }

  /** Exercise 5.14 */
  test("startsWith") {
    val long = Stream(1,2,3)
    val short = Stream(1,2)

    assert(long.startsWith(short))
    assert(!short.startsWith(long))
  }

  /** Exercise 5.15 */
  test("tails") {
    val stream = Stream(1,2,3)
    val tails = Stream(Stream(1,2,3), Stream(2,3), Stream(3))
    assert(stream.tails.map(_.toList).toList == tails.map(_.toList).toList)
  }

  test("hasSubsequence") {
    val long = Stream(1,2,3,4)
    val short = Stream(2,3)
    assert(long.hasSubsequence(short))
    assert(!short.hasSubsequence(long))
  }

  /** Exercise 5.16 */
  test("scanRight") {
    val stream = Stream(1,2,3)
    //assert(stream.scanRight(0)(_+_).toList == List(6, 3, 1))
  }
}
