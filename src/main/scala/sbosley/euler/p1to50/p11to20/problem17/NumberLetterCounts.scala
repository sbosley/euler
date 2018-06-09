package sbosley.euler.p1to50.p11to20.problem17

object NumberLetterCounts {

  def main(args: Array[String]): Unit = {
    println((1 to 1000).map(numberToWord(_).length).sum)
  }

  def numberToWord(n: Int): String = {
    if (10000 > n && n >= 1000) {
      numberToWord(n / 1000) + "thousand" + numberToWord(n % 1000)
    } else if (1000 > n && n >= 100) {
      val hundreds = lowNToWord(n / 100) + "hundred"
      val rest = numberToWord(n % 100)
      if (rest.isEmpty) hundreds
      else hundreds + "and" + rest
    } else {
      tensToWord(n)
    }
  }

  def tensToWord(n: Int): String = {
    if (n >= 90) "ninety" + lowNToWord(n % 10)
    else if (n >= 80) "eighty" + lowNToWord(n % 10)
    else if (n >= 70) "seventy" + lowNToWord(n % 10)
    else if (n >= 60) "sixty" + lowNToWord(n % 10)
    else if (n >= 50) "fifty" + lowNToWord(n % 10)
    else if (n >= 40) "forty" + lowNToWord(n % 10)
    else if (n >= 30) "thirty" + lowNToWord(n % 10)
    else if (n >= 20) "twenty" + lowNToWord(n % 10)
    else lowNToWord(n)
  }

  def lowNToWord(n: Int): String = {
    if (n == 1) "one"
    else if (n == 2) "two"
    else if (n == 3) "three"
    else if (n == 4) "four"
    else if (n == 5) "five"
    else if (n == 6) "six"
    else if (n == 7) "seven"
    else if (n == 8) "eight"
    else if (n == 9) "nine"
    else if (n == 10) "ten"
    else if (n == 11) "eleven"
    else if (n == 12) "twelve"
    else if (n == 13) "thirteen"
    else if (n == 14) "fourteen"
    else if (n == 15) "fifteen"
    else if (n == 16) "sixteen"
    else if (n == 17) "seventeen"
    else if (n == 18) "eighteen"
    else if (n == 19) "nineteen"
    else ""
  }

}
