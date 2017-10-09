/** Chapter 2 examples and exercises */
object MyModule {
  def main(args: Array[String]): Unit = {
    val arg: Int = args(0).toInt

    println(formatResult("absolute value", arg, abs))
    println(formatResult("factorial", arg, factorial))
    println(formatResult("fibonacci", arg, fibonacci))

    println(isSorted(Array(1, 2, 1), (a: Int, b: Int) => a <= b))
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int): Int = {
      if (n <= 0) a
      else go(n - 1, n * a)
    }

    go(n, 1)
  }

  /** Exercise 2.1 */
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def fib(n: Int, a: Int, b: Int): Int = {
      if (n < 2) a
      else fib(n - 1, a + b, a)
    }

    fib(n, 1, 1)
  }

  /** Listing 2.2 */
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    "The %s of %d is %d".format(name, n, f(n))
  }

  /** Listing 2.3, 2.4 */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /** Exercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def sorted(arr: Array[A], acc: Boolean): Boolean = {
      if (!acc || arr.length < 2) acc
      else {
        val Array(a, b, _*) = arr
        sorted(arr.tail, acc && ordered(a, b))
      }
    }

    sorted(as, true)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  /** Exercise 2.3 */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partial1(a, f)

  /** Exercise 2.4 */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /** Exercise 2.5*/
  def compose[A, B, C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))
}
