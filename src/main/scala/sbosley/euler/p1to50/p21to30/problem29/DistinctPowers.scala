package sbosley.euler.p1to50.p21to30.problem29

object DistinctPowers {

  def main(args: Array[String]): Unit = {
    val distinctPowers = (for {
      a <- 2 to 100
      b <- 2 to 100
    } yield {
      BigInt(a).pow(b)
    }).toSet
    println(distinctPowers.size)
  }

}
