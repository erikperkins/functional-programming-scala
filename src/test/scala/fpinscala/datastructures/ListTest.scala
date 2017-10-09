package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("sum sums a list of integers") {
    val list = List(1, 2, 3)
    assert(List.sum(list) == 6)
  }

  test("product finds the product of doubles") {
    val list = List(1.2, 3.4, 5.6)
    assert(List.product(list) == 22.848)
  }

  test("apply applies things") {
    val list = List()
    assert(list == Nil)
  }

  /** Exercise 3.1 */
  test("matchList matches the correct pattern") {
    val list = List(1, 2, 3, 4, 5)
    assert(List.matchList(list) == 3)
  }


  /** Exercise 3.2 */
  test("tail returns the tail") {
    val list = List(1, 2, 3)
    val shortList = List(1)
    val emptyList = List()

    assert(List.tail(list) == List(2, 3))
    assert(List.tail(shortList) == Nil)
    assert(List.tail(emptyList) == Nil)
  }


  /** Exercise 3.3 */
  test("setHead resets the head") {
    val head = 10
    val list = List(1, 2, 3)

    assert(List.setHead(list, head) == List(head, 2, 3))
    assert(List.setHead(Nil, head) == List(head))
  }

  /** Exercise 3.4 */
  test("drop removes first n elements") {
    val list = List(1, 2, 3, 4, 5)

    assert(List.drop(list, 3) == List(4, 5))
    assert(List.drop(list, 100) == Nil)
    assert(List.drop(list, 0) == list)
  }


  /** Exercise 3.5 */
  test("dropWhile removes head until predicate is false") {
    val list = List(1, 2, 3, 4, 5)

    assert(List.dropWhile(list, (a: Int) => a < 3) == List(3, 4, 5))
    assert(List.dropWhile(list, (a: Int) => a < 6) == Nil)
    assert(List.dropWhile(list, (a: Int) => a > 2) == list)
  }

  test("append appends") {
    val first = List(1, 2)
    val last = List(3, 4)

    assert(List.append(first, last) == List(1, 2, 3, 4))
    assert(List.append(first, Nil) == first)
    assert(List.append(Nil, last) == last)
    assert(List.append(Nil, Nil) == Nil)
  }

  /** Exercise 3.6 */
  test("init takes first n elements") {
    val list = List(1, 2, 3)

    assert(List.init(list, 2) == List(1, 2))
    assert(List.init(list, 10) == list)
    assert(List.init(list, 0) == Nil)
  }

  /** Exercise 3.9 */
  test("length measures length") {
    val list = List(1, 2, 3)

    assert(List.length(list) == 3)
    assert(List.length(Nil) == 0)
  }

  /** Exercise 3.12 */
  test("reverse reverses") {
    val list = List(1, 2, 3, 4)

    assert(List.reverse(list) == List(4, 3, 2, 1))
  }

  /** Exercise 3.15 */
  test("accrete accretes") {
    val list = List(Nil, List(1), List(2, 3), List(4, 5, 6))

    assert(List.accrete(list) == List(1, 2, 3, 4, 5, 6))
    assert(List.accrete(List(Nil)) == Nil)
  }

  /** Exercise 3.16 */
  test("increment increments") {
    val list = List(1, 2, 3)

    assert(List.increment(list) == List(2, 3, 4))
    assert(List.increment(Nil) == Nil)
  }

  /** Exercise 3.17 */
  test("stringify stringifies") {
    val list = List(1.2, 3.4, 5.6)

    assert(List.stringify(list) == List("1.2", "3.4", "5.6"))
    assert(List.stringify(Nil) == Nil)
  }

  /** Exercise 3.18 */
  test("map maps") {
    val list = List(1, 2, 3)
    val f = (a: Int) => a + 10

    assert(List.map(list)(f) == List(11, 12, 13))
    assert(List.map(Nil)(f) == Nil)
  }

  /** Exercise 3.19 */
  test("filter filters") {
    val list = List(1, 2, 3, 4)
    val p = (a: Int) => a % 2 == 0

    assert(List.filter(list)(p) == List(2, 4))
    assert(List.filter(Nil)(p) == Nil)
  }

  /** Exercise 3.20 */
  test("flatMap returns a flattened list") {
    val list = List(1, 2, 3)
    val f = (a: Int) => List(a, a)

    assert(List.flatMap(list)(f) == List(1, 1, 2, 2, 3, 3))
    assert(List.flatMap(Nil)(f) == Nil)
  }

  /** Exercise 3.21 */
  test("select selects") {
    val list = List(1, 2, 3, 4)
    val p = (a: Int) => a % 2 == 0

    assert(List.select(list)(p) == List(2, 4))
    assert(List.select(Nil)(p) == Nil)
  }

  /** Exercise 3.22 */
  test("zipWithAdd") {
    val l = List(1, 2, 3)
    val m = List(4, 5, 6)
    val n = List(7)

    assert(List.zipWithAdd(l, m) == List(5, 7, 9))
    assert(List.zipWithAdd(m, n) == List(11))
    assert(List.zipWithAdd(l, Nil) == Nil)
  }

  /** Exercise 3.23 */
  test("zipWith") {
    val l = List("a: ", "b: ", "c: ")
    val m = List(1.2, 3.4, 5.6)
    val n = List(7.8)
    val f = (a: String, b: Double) => a + b.toString

    assert(List.zipWith(l, m)(f) == List("a: 1.2", "b: 3.4", "c: 5.6"))
    assert(List.zipWith(l, n)(f) == List("a: 7.8"))
  }
}
