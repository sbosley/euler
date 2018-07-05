package sbosley.euler.p1to50.p21to30.problem25

import sbosley.euler.util.math.MathHelpers

object OneThousandDigitFibonacci {

  def main(args: Array[String]): Unit = {
    val (_, i) = MathHelpers.FIBONACCI_STREAM.zipWithIndex.find { case (f, _) =>
      f.toString.length == 1000
    }.get
    println(i) // Our fibonacci sequence includes 0
  }
}
