package sbosley.euler.p51to100.p61to70.problem62

object CubicPermutations {

  def main(args: Array[String]): Unit = {
    val cubes = (1 to 100000).map(n => BigInt(n).pow(3)).groupBy(canonicalPermutation)
    val cubesWith5Permutations = cubes.filter(_._2.size == 5)
    println(cubesWith5Permutations.values.flatten.min)
  }

  private def canonicalPermutation(n: BigInt): String = n.toString.sorted

}
