package sbosley.euler.p1to50.p31to40.problem38

object PandigitalMultiples {
  // concatenated product of x and (1, 2, ... n)
  // x * 1 :: x * 2 :: x * 3 :: x * 4 ... :: x * n
  // we know that 918273645 is possible with x = 9 and n = 5
  // x cannot be a 5 digit number, because 9123 (the smallest 4 digit pandigital starter) * 2 has
  // n cannot be > 9 / x's digits

  private val chars = "123456789".toSet

  def main(args: Array[String]): Unit = {
    val pandigitals = for {
      x <- 1 to 9999
      n <- 2 until 9
      c = concatenatedProduct(x, n)
      if isPandigital(c)
    } yield {
      (x, n, c)
    }
    println(pandigitals.maxBy(_._3.toInt))
  }

  private def concatenatedProduct(x: Int, n: Int): String = {
    (1 to n).foldLeft("") { (str, i) => str + x * i }
  }

  private def isPandigital(str: String): Boolean = {
    str.length == chars.size && str.toSet == chars
  }
}
