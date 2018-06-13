package sbosley.euler.p1to50.p31to40.problem39

import sbosley.euler.math.Primes

object IntegerRightTriangles {

  // a^2 + b^2 = c^2
  // a + b + c = p
  // Euclid's formula doesn't generate all possible triples without the k factor
  // a = k * (m^2 - n^2), b = k * 2mn, c = k * m^2 + n^2 where m > n > 0; k > 0; m, n coprime and not both odd.
  // k * 2m * (m + n) = p

  def main(args: Array[String]): Unit = {
    val pWithMost = (2 to 1000 by 2).maxBy(findPythagoreanTriples(_).size)
    println(pWithMost)
  }

  private def findPythagoreanTriples(p: Int): Set[Set[Int]] = {
    // k * m * (m + n) = p / 2 where m > n > 0, k > 0, m and n are coprime and not both odd
    val halfP = p / 2
    val possibleKs = Primes.properDivisors(p / 2)
    possibleKs.foldLeft(Set.empty[Set[Int]]) { (set, k) =>
      val pOver2k = halfP / k
      val possibleMs = Primes.properDivisors(pOver2k)
      val possibleMNpairs = possibleMs.map(m => (m, pOver2k / m - m)).filter {
        case (m, n) => m > 0 && n > 0 && m > n
      }
      val triples = possibleMNpairs.foldLeft(Set.empty[Set[Int]]) { case (setForK, (m, n)) =>
        val a = k * (m * m - n * n)
        val b = k * 2 * m * n
        val c = k * (m * m + n * n)
        if (a + b + c != p) throw new IllegalStateException(s"$a + $b + $c should = $p")
        if (isPythagoreanTriple(a, b, c)) {
          setForK + Set(a, b, c)
        } else setForK
      }
      set ++ triples
    }
  }

  private def isPythagoreanTriple(a: Int, b: Int, c: Int): Boolean = {
    a * a + b * b == c * c
  }

}
