package sbosley.euler.p1to20.problem9

object Problem9 {

  def main(args: Array[String]): Unit = {
    // Euclid's formula: a = m^2 - n^2, b=2mn, c=m^2 + n^2 form a pythagorean triple for any m, n where m > n > 0
    // Find a pythagorean triple s.t. a + b + c = 1000
    // m^2 - n^2 + 2mn + m^2 + n^2 = 1000
    // 2m^2 + 2mn = 1000
    // 2m * (m + n) = 1000
    // m * (m + n) = 500
    // m = 20, n = 5
    // a = 375
    // b = 200
    // c = 425
    // abc = 31875000
  }

}
