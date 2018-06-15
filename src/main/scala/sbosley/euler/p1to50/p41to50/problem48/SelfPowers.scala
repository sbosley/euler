package sbosley.euler.p1to50.p41to50.problem48

object SelfPowers {

  def main(args: Array[String]): Unit = {
    val sum = (1 to 1000).map(i => BigInt(i).pow(i)).sum
    val last10Digits = sum % BigInt(10000000000L)
    println(last10Digits.toString)
  }

}
