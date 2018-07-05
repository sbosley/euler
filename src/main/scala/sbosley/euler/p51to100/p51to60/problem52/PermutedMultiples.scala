package sbosley.euler.p51to100.p51to60.problem52

import sbosley.euler.util.string.StringExt._

object PermutedMultiples {

  def main(args: Array[String]): Unit = {
    val ranges = Stream.from(1)
      .map(p => BigInt(10).pow(p))
      .flatMap(pow => pow to BigInt(pow.toString.replace('0', '6')))

    val result = ranges.find(x => {
      val xString = x.toString
      (2 to 6).forall(i => xString.isPermutation((x * i).toString))
    })
    println(result)
  }

}
