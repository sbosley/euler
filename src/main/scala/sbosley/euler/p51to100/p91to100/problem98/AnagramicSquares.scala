package sbosley.euler.p51to100.p91to100.problem98

import scala.collection.immutable.ListMap
import scala.io.Source

object AnagramicSquares {

  type CanonicalEncoding = ListMap[Char, Seq[Int]]
  type CanonicalPair = (CanonicalEncoding, CanonicalEncoding)

  def main(args: Array[String]): Unit = {
    // Replaced commas and quotes with newlines:
    //    sed -i '' 's/,/\
    //    /g' <file>
    val words = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p91to100/problem98/p098_words.txt").getLines.toSeq

    val wordAnagramicPairs = words.foldLeft(Map.empty[String, Set[String]]) { (acc, word) =>
      val key = word.sorted
      acc + (key -> (acc.getOrElse(key, Set.empty) + word))
    }.filter { case (_, value) => value.size > 1 }.values.toList

    val maxWordLength = wordAnagramicPairs.flatten.maxBy(_.length).length
    val squaresByLength = Stream.iterate(1L)(_ + 1)
      .map(n => n * n)
      .takeWhile(_.toString.length <= maxWordLength)
      .foldLeft(Map.empty[Int, Set[Long]]) { (acc, square) =>
        val key = square.toString.length
        acc + (key -> (acc.getOrElse(key, Set.empty) + square))
      }

    // For each word anagramic pair, find (if it exists) a square anagramic pair with matching transformations
    val result =
      wordAnagramicPairs
        .sortBy(_.head.length)
        .reverse
        .flatMap(allPairs)
        .flatMap { case (a, b) => toAnagramicSquarePair(a, b, squaresByLength) }
        .flatten
        .max
    println(result)
  }

  private def allPairs[T](set: Set[T]): Set[(T, T)] = {
    for {
      item1 <- set
      item2 <- set
      if item1 != item2
    } yield {
      (item1, item2)
    }
  }

  private def allIndexesOf(c: Char, word: String): Seq[Int] = {
    word.zipWithIndex.filter(_._1 == c).map(_._2)
  }

  private def canonicalEncoding(word: String): CanonicalEncoding = {
    word.zipWithIndex.foldLeft(ListMap.empty[Char, Seq[Int]]) { case (acc, (char, index)) =>
      acc + (char -> (acc.getOrElse(char, Seq.empty) :+ index))
    }
  }

  private def anagramicMapping(a: String, b: String): CanonicalEncoding = {
    val canonical = canonicalEncoding(a)
    canonical.keys.foldLeft(ListMap.empty[Char, Seq[Int]]) { (acc, char) =>
      acc + (char -> allIndexesOf(char, b))
    }
  }

  private def canonicalEncodingString(str: String): String = {
    val encoding = canonicalEncoding(str)
    val result = encoding.values.foldLeft(StringBuilder.newBuilder) { (builder, indices) =>
      builder.append("(")
      indices.foldLeft(builder) { (b, i) =>
        b.append(i).append(",")
      }
      builder.deleteCharAt(builder.length - 1).append("),")
    }
    result.deleteCharAt(result.length - 1).toString
  }

  private def toAnagramicSquarePair(a: String, b: String, squaresByLength: Map[Int, Set[Long]]): Set[Set[Long]] = {
    val candidates = squaresByLength.getOrElse(a.length, Set.empty)
    val mapping = anagramicMapping(a, b)
    candidates.flatMap { square =>
      val squareStr = square.toString
      if (canonicalEncodingString(a) == canonicalEncodingString(squareStr)) {
        val anagramicSquare = applyAnagramicMapping(squareStr, mapping).toLong
        if (candidates(anagramicSquare)) {
          Some(Set(square, anagramicSquare))
        } else None
      } else None
    }
  }

  private def applyAnagramicMapping(word: String, anagramicMapping: CanonicalEncoding): String = {
    anagramicMapping.zipWithIndex.foldLeft(new Array[Char](word.length)) { case (newWord, ((_, indices), index)) =>
      val char = word(index)
      indices.foreach(newWord.update(_, char))
      newWord
    }.mkString
  }
}
