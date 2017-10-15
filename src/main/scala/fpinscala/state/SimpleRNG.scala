package fpinscala.state

sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  /** Exercise 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (Math.abs(n % Int.MaxValue), r)
  }

  /** Exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, r)
  }

  /** Exercise 6.3 */
  def intDouble(rng: RNG): (Int, Double) = {
    val (n, rng1) = rng.nextInt
    val (d, _) = double(rng1)
    (n, d)
  }

  /** Exercise 6.3 */
  def doubleInt(rng: RNG): (Double, Int) = {
    val (d, rng1) = double(rng)
    val (n, _) = rng1.nextInt
    (d, n)
  }

  /** Exercise 6.3 */
  def tripleDouble(rng: RNG): (Double, Double, Double) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, _) = double(rng2)
    (d1, d2, d3)
  }

  /** Exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def int(n: Int, l: List[Int], r: RNG): (List[Int], RNG) = r.nextInt match {
      case (i, r1) if n > 0 => int(n - 1, i :: l, r1)
      case _ => (l, r)
    }

    int(count, List(), rng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](r: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = r(rng)
      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /** Exercise 6.5 */
  def _double(rng: RNG): Double =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)(rng)._1

  /** Exercise 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_,_))

  def _intDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)

  def _doubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  /** Exercise 6.7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val end = unit[List[A]](Nil)
    fs.foldRight(end)((r, p) => map2(r, p)(_::_))
  }

  /** Exercise 6.7 */
  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))

  /** Exercise 6.8 */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  /** Exercise 6.8 */
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => if (i < n) unit(i) else nonNegativeLessThan(n))

  /** Exercise 6.9 */
  def _map[A,B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  /** Exercise 6.9 */
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}
