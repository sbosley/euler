package sbosley.euler.p1to50.p31to40.problem31

object CoinSums {

  case class CoinCounts(twoHundreds: Int = 0,
                        oneHundreds: Int = 0,
                        fifties: Int = 0,
                        twenties: Int = 0,
                        tens: Int = 0,
                        fives: Int = 0,
                        twos: Int = 0,
                        ones: Int = 0)

  private def sumCoins(twoHundreds: Int = 0,
                       oneHundreds: Int = 0,
                       fifties: Int = 0,
                       twenties: Int = 0,
                       tens: Int = 0,
                       fives: Int = 0,
                       twos: Int = 0,
                       ones: Int = 0): Int = {
    200 * twoHundreds +
      100 * oneHundreds +
      50 * fifties +
      20 * twenties +
      10 * tens +
      5 * fives +
      2 * twos +
      ones

  }

  def main(args: Array[String]): Unit = {
    val counts = for {
      twoHundreds <- 0 to 1
      oneHundreds <- 0 to (200 - sumCoins(twoHundreds)) / 100
      fifties <- 0 to (200 - sumCoins(twoHundreds, oneHundreds)) / 50
      twenties <- 0 to (200 - sumCoins(twoHundreds, oneHundreds, fifties)) / 20
      tens <- 0 to (200 - sumCoins(twoHundreds, oneHundreds, fifties, twenties)) / 10
      fives <- 0 to (200 - sumCoins(twoHundreds, oneHundreds, fifties, twenties, tens)) / 5
      twos <- 0 to (200 - sumCoins(twoHundreds, oneHundreds, fifties, twenties, tens, fives)) / 2
      ones = 200 - sumCoins(twoHundreds, oneHundreds, fifties, twenties, tens, fives, twos)
    } yield {
      CoinCounts(twoHundreds, oneHundreds, fifties, twenties, tens, fives, twos, ones)
    }
    println(counts.size)
  }
}