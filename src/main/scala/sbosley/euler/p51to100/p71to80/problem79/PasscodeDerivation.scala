package sbosley.euler.p51to100.p71to80.problem79

import scala.annotation.tailrec
import scala.io.Source

object PasscodeDerivation {

  def main(args: Array[String]): Unit = {
    val attempts = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p71to80/problem79/p079_keylog.txt").getLines.toSeq
    println(buildPasscodeRecursive(attempts, List.empty))
  }

  @tailrec
  private def buildPasscodeRecursive(attempts: Seq[String], accumulatedChars: List[Char]): String = {
    if (attempts.isEmpty) accumulatedChars.reverse.mkString
    else {
      val firstPositions = attempts.flatMap[Char, Set[Char]](a => a.lift(0))(collection.breakOut)
      val secondPositions = attempts.flatMap[Char, Set[Char]](a => a.lift(1))(collection.breakOut)
      val nextChar = firstPositions.filterNot(secondPositions).head
      val updatedAttempts = attempts.flatMap(a => {
        if (a.length == 1 && a == nextChar.toString) None
        else if (a.startsWith(nextChar.toString)) Some(a.substring(1))
        else Some(a)
      })
      buildPasscodeRecursive(updatedAttempts, nextChar :: accumulatedChars)
    }
  }

}
