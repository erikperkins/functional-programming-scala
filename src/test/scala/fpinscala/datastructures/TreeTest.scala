package fpinscala.datastructures

import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  /** Exercise 3.25 */
  test("size") {
    val leaf = Leaf("a")
    val tree = Branch(Leaf("b"), Leaf("c"))
    val bigTree = Branch(Branch(tree, tree), leaf)

    assert(Tree.size(leaf) == 1)
    assert(Tree.size(tree) == 3)
    assert(Tree.size(bigTree) == 9)
  }

  /** Exercise 3.26 */
  test("maximum") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.maximum(tree) == 3)
  }

  /** Exercise 3.27 */
  test("depth") {
    val tree = Branch(Branch(Branch(Leaf(0), Leaf(0)), Leaf(0)), Leaf(0))

    assert(Tree.depth(tree) == 3)
    assert(Tree.depth(Leaf(0)) == 0)
  }

  /** Exercise 3.28 */
  test("map") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a + 1

    assert(Tree.map(tree)(f) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  }

  /** Exercise 3.29 */
  test("fold") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a
    val g = (b: Int, c: Int) => b + c

    assert(Tree.fold(tree)(f, g) == 6)
  }

}

