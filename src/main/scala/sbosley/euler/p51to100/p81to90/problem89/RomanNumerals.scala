package sbosley.euler.p51to100.p81to90.problem89

import scala.io.Source

object RomanNumerals {

  // Numerals must be arranged in descending order of size.
  // M, C, and X cannot be equalled or exceeded by smaller denominations.
  // D, L, and V can each only appear once.

  // Only one I, X, and C can be used as the leading numeral in part of a subtractive pair.
  // I can only be placed before V and X.
  // X can only be placed before L and C.
  // C can only be placed before D and M.

  private val numeralMap = Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

  def main(args: Array[String]): Unit = {
    val numerals = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p81to90/problem89/p089_roman.txt").getLines.toSeq
    val intValues = numerals.map(n => parseRomanNumeral(n.toList))
    val minimalNumerals = intValues.map(toRomanNumeral(_))

    println(s"Char difference ${charCount(numerals) - charCount(minimalNumerals)}")
  }

  private def parseRomanNumeral(chars: List[Char], sumSoFar: Int = 0): Int = {
    if (chars.isEmpty) {
      sumSoFar
    } else {
      val headCharValue = numeralMap(chars.head)
      val remainingChars = chars.tail
      if (remainingChars.isEmpty) {
        sumSoFar + headCharValue
      } else {
        val nextCharValue = numeralMap(remainingChars.head)
        if (nextCharValue > headCharValue) {
          parseRomanNumeral(remainingChars.tail, sumSoFar + (nextCharValue - headCharValue))
        } else {
          parseRomanNumeral(remainingChars, sumSoFar + headCharValue)
        }
      }
    }
  }

  private def toRomanNumeral(remaining: Int, charsSoFar: List[Char] = List.empty): String = {
    if (remaining >= 1000) {
      toRomanNumeral(remaining - 1000, 'M' :: charsSoFar)
    } else if (remaining >= 900) {
      toRomanNumeral(remaining - 900, 'M' :: 'C' :: charsSoFar)
    } else if (remaining >= 500) {
      toRomanNumeral(remaining - 500, 'D' :: charsSoFar)
    } else if (remaining >= 400) {
      toRomanNumeral(remaining - 400, 'D' :: 'C' :: charsSoFar)
    } else if (remaining >= 100) {
      toRomanNumeral(remaining - 100, 'C' :: charsSoFar)
    } else if (remaining >= 90) {
      toRomanNumeral(remaining - 90, 'C' :: 'X' :: charsSoFar)
    } else if (remaining >= 50) {
      toRomanNumeral(remaining - 50, 'L' :: charsSoFar)
    } else if (remaining >= 40) {
      toRomanNumeral(remaining - 40, 'L' :: 'X' :: charsSoFar)
    } else if (remaining >= 10) {
      toRomanNumeral(remaining - 10, 'X' :: charsSoFar)
    } else if (remaining >= 9) {
      toRomanNumeral(remaining - 9, 'X' :: 'I' :: charsSoFar)
    } else if (remaining >= 5) {
      toRomanNumeral(remaining - 5, 'V' :: charsSoFar)
    } else if (remaining >= 4) {
      toRomanNumeral(remaining - 4, 'V' :: 'I' :: charsSoFar)
    } else if (remaining > 0) {
      toRomanNumeral(remaining - 1, 'I' :: charsSoFar)
    } else {
      charsSoFar.reverse.mkString
    }
  }

  private def charCount(strings: Seq[String]): Int = {
    strings.map(_.length).sum
  }

}
