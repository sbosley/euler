package sbosley.euler.p1to20.problem1

object Problem1 {

  def main(args: Array[String]): Unit = {
    println(sumTo1000)
  }

  def sumTo1000: Int = {
    (1 until 1000 filter { i => i % 3 == 0 || i % 5 == 0 }).sum
  }

}
