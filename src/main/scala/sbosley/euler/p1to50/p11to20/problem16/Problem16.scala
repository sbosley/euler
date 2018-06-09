package sbosley.euler.p1to50.p11to20.problem16

object Problem16 {

  def main(args: Array[String]): Unit = {
    println(BigInt(2).pow(1000).toString.toCharArray.map(_.asDigit).sum)
  }

}
