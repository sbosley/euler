package sbosley.euler.p1to50.p31to40.problem32

object PandigitalProducts {

  private val completeDigitSet = "123456789".toList

  def main(args: Array[String]): Unit = {
    // Things to check:
    // 1 digit x 4 digit = 4 digit
    // 2 digit x 3 digit = 4 digit
    // 2 digit x 4 digit = 3 digit
    // 3 digit x 3 digit = 3 digit
    val pandigitalProducts =
      possibleCharSets(2 to 9, 1000 to 9999).toSet ++
        possibleCharSets(10 to 99, 100 to 9999).toSet ++
        possibleCharSets(100 to 999, 100 to 999).toSet
    println(pandigitalProducts.sum)
  }

  private def possibleCharSets(aRange: Range, bRange: Range): Seq[Int] = {
    for {
      a <- aRange
      b <- bRange
      c = a * b
      digits = a.toString + b.toString + c.toString
      if digits.sorted.toList == completeDigitSet
    } yield {
      println(s"$a * $b = $c")
      c
    }
  }
}
