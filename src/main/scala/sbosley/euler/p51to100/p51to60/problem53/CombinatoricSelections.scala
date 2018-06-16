package sbosley.euler.p51to100.p51to60.problem53

object CombinatoricSelections {

  private val max = 100
  private val factorials = (1 to max).map(BigInt(_)).scanLeft(BigInt(1)) { (acc, n) => acc * n}
    .tail.zipWithIndex.map({ case (value, idx) => idx + 1 -> value }).toMap + (0 -> BigInt(1))

  def main(args: Array[String]): Unit = {
    val count = (1 to max).flatMap(n => (1 to n).map(r => choose(n, r))).count(_ > 1000000)
    println(count)
  }

  def choose(n: Int, r: Int): BigInt = {
    factorials(n) / (factorials(r) * factorials(n - r))
  }
}