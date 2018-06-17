package sbosley.euler.p51to100.p51to60.problem56

object PowerfulDigitSum {

  def main(args: Array[String]): Unit = {
    val sums = for {
      a <- 1 until 100
      b <- 1 until 100
    } yield {
      digitSum(BigInt(a).pow(b))
    }
    println(sums.max)
  }

  private def digitSum(n: BigInt): Long = {
    n.toString.map(_.asDigit).sum
  }
}
