import org.scalatest.FunSuite

class MyModuleTest extends FunSuite {

  test("main prints absolute value of command line argument") {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      MyModule.main(Array("-42"))
    }

    assert(stream.toString() contains "The absolute value of -42 is 42")
  }

  test("main prints factorial of command line argument") {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      MyModule.main(Array("3"))
    }

    assert(stream.toString() contains "The factorial of 3 is 6")
  }

  test("main prints fibonnaci of command line argument") {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      MyModule.main(Array("6"))
    }

    assert(stream.toString() contains "The fibonacci of 6 is 13")
  }

  test("abs negates negative numbers") {
    val absoluteValue: Int = MyModule.abs(-42)
    assert(absoluteValue == 42)
  }

  test("abs returns positive numbers") {
    val absoluteValue: Int = MyModule.abs(42)
    assert(absoluteValue == 42)
  }

  test("factorial(n) returns n!") {
    val factorial = MyModule.factorial(4)
    assert(factorial == 24)
  }

  test("factorial(n) returns 1 for n < 0") {
    val factorial = MyModule.factorial(-4)
    assert(factorial == 1)
  }

  test("fibonacci(n) returns nth fibonacci number") {
    val fibonaccis = Array(1, 1, 2, 3, 5, 8, 13)
    val output = (0 to 6).map(MyModule.fibonacci).toArray

    assert(output.deep == fibonaccis.deep)
  }

  test("formatResult returns the proper response") {
    val formatResult = MyModule.formatResult("square", 3, a => a*a)
    assert(formatResult contains "The square of 3 is 9")
  }

  test("findFirst returns index of first occurrence") {
    val array = Array("hello", "world", "world")
    val firstIndex = MyModule.findFirst(array, (a: String) => a == "world")
    assert(firstIndex == 1)
  }

  test("findFirst returns -1 if key not found") {
    val array = Array(1, 2)
    val firstIndex = MyModule.findFirst(array, (a: Int) => a == 3)
    assert(firstIndex == -1)
  }

  test("isSorted returns true for empty array") {
    val array = Array()
    val sorted = MyModule.isSorted(array, (a: Int, b: Int) => a <= b)

    assert(sorted)
  }

  test("isSorted returns true for ordered array") {
    val ordering: (Int, Int) => Boolean = _ <= _
    val array = Array(1, 2, 3)
    val sorted = MyModule.isSorted(array, ordering)

    assert(sorted)
  }

  test("isSorted returns false for unordered array") {
    val ordering: (Int, Int) => Boolean = _ <= _
    val array = Array(2, 1)
    val sorted = MyModule.isSorted(array, ordering)

    assert(!sorted)
  }

  test("partial1 partially applies an argument") {
    val f: (Int, Int) => Boolean = (a, b) => a <= b
    val positive: Int => Boolean = MyModule.partial1(0, f)

    assert(positive(3))
  }

  test("curry curries functions") {
    val plus = (a: Int, b: Int) => a + b
    val curriedPlus = MyModule.curry(plus)

    assert(curriedPlus(1)(2) == 3)
  }

  test("uncurry uncurries functions") {
    val curriedPlus = MyModule.curry((a: Int, b: Int) => a + b)
    val plus = MyModule.uncurry(curriedPlus)

    assert(plus(1, 2) == 3)
  }

  test("compose composes functions") {
    val plusOne = (a: Int) => a + 1
    val overTwo = (a: Int) => a / 2.0
    val plusOneOverTwo = MyModule.compose(plusOne, overTwo)

    assert(plusOneOverTwo(4) == 2.5)
  }
}