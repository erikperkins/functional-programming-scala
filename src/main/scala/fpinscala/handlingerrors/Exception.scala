package fpinscala.handlingerrors

object Exception {
  def failingFunction(i: Int): Int = {
    //val y: Int = throw new Exception("fail")
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Double = {
    if (xs.isEmpty) throw new ArithmeticException("mean of empty list")
    else xs.sum / xs.length
  }

  def main(args: Array[String]): Unit = {
    println(failingFunction(12))
  }
}
