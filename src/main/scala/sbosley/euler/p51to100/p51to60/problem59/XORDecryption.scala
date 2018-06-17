package sbosley.euler.p51to100.p51to60.problem59

import scala.io.Source

object XORDecryption {

  def main(args: Array[String]): Unit = {
    // Replaced commas with newlines:
    //    sed -i '' 's/,/\
    //    /g' <file>
    val fileChars = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p51to60/problem59/p059_cipher.txt")
      .getLines.map(_.toInt).toSeq

    val maybeKeys = for {
      a <- ('a' to 'z').map(_.toInt)
      b <- ('a' to 'z').map(_.toInt)
      c <- ('a' to 'z').map(_.toInt)
      if quickTestKey(fileChars, a, b, c)
    } yield {
      (a, b, c)
    }
    val asciiSum = maybeKeys.headOption.map { case (a, b, c) =>
      val decoded = decode(fileChars, a, b, c)
      println(decoded)
      decoded
    }.getOrElse("").map(_.toInt).sum
    println(asciiSum)
  }

  private def quickTestKey(fileChars: Seq[Int], a: Int, b: Int, c: Int): Boolean = {
    Seq(
      fileChars(0) ^ a, fileChars(1) ^ b, fileChars(2) ^ c, fileChars(3) ^ a)
      .map(_.toChar).mkString.contains("The") // Some trial and error determined that the file started with "(The"
  }

  private def decode(fileChars: Seq[Int], a: Int, b: Int, c: Int): String = {
    (0 to fileChars.length by 3).flatMap(idx => {
      val char1 = fileChars.lift(idx).map(_ ^ a).map(_.toChar).getOrElse(' ')
      val char2 = fileChars.lift(idx + 1).map(_ ^ b).map(_.toChar).getOrElse(' ')
      val char3 = fileChars.lift(idx + 2).map(_ ^ c).map(_.toChar).getOrElse(' ')
      Seq(char1, char2, char3)
    }).mkString.trim
  }
}
