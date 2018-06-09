package sbosley.euler.p1to50.p21to30.problem22

import java.io.File

import scala.io.Source

object Problem22 {

  def main(args: Array[String]): Unit = {
    // Replaced commas with newlines:
    //    sed -i '' 's/,/\
    //    /g' src/main/scala/sbosley/euler/p1to50/p21to30/problem22/p022_names.txt
    val sortedNames = Source.fromFile("src/main/scala/sbosley/euler/p1to50/p21to30/problem22/p022_names.txt").getLines.toSeq.sorted
    val nameScoreSum = sortedNames.zipWithIndex.map { case (name, index) =>
      val nameScore = name.replaceAll("\"", "").map(c => c - 'A' + 1).sum
      BigInt(nameScore) * (index + 1)
    }.sum
    println(nameScoreSum)
  }

}
