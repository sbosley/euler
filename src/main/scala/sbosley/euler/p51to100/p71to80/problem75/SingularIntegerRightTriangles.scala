package sbosley.euler.p51to100.p71to80.problem75

import sbosley.euler.util.math.Primes

import scala.annotation.tailrec
import scala.collection.mutable

object SingularIntegerRightTriangles {

  private val MAX_PERIMETER = 1500000

  private val primes = Primes.primesToMax(MAX_PERIMETER + 100)
  case class PythagoreanTriple(a: Int, b: Int, c: Int) {
    lazy val perimeter: Int = a + b + c
  }

  def main(args: Array[String]): Unit = {
    val allTriples = generateChildrenRecursively(Seq(PythagoreanTriple(3, 4, 5)), mutable.Map(12 -> mutable.Set(PythagoreanTriple(3, 4, 5))))
    val lengths = 12 to MAX_PERIMETER by 2
    println(lengths.count(l => {
      val divisors = Primes.allDivisors(l, Some(primes))
      divisors.count(allTriples.contains) == 1
    }))
  }

  @tailrec
  private def generateChildrenRecursively(triples: Seq[PythagoreanTriple],
                                          acc: mutable.Map[Int, mutable.Set[PythagoreanTriple]]): Map[Int, Set[PythagoreanTriple]] = {
    val newTriples = triples.flatMap(t => Seq(childA(t), childB(t), childC(t)).filter(t => t.perimeter < MAX_PERIMETER))
    if (newTriples.isEmpty) acc.mapValues(_.toSet).toMap
    else {
      newTriples.foreach(t => {
        acc.getOrElseUpdate(t.perimeter, mutable.Set.empty) += t
      })
      generateChildrenRecursively(newTriples, acc)
    }
  }

  private def childA(triple: PythagoreanTriple): PythagoreanTriple = {
    val a1 = triple.a - 2*triple.b + 2*triple.c
    val b1 = 2*triple.a - triple.b + 2*triple.c
    val c1 = 2*triple.a - 2*triple.b + 3*triple.c
    PythagoreanTriple(a1, b1, c1)
  }

  private def childB(triple: PythagoreanTriple): PythagoreanTriple = {
    val a2 = triple.a + 2*triple.b + 2*triple.c
    val b2 = 2*triple.a + triple.b + 2*triple.c
    val c2 = 2*triple.a + 2*triple.b + 3*triple.c
    PythagoreanTriple(a2, b2, c2)
  }

  private def childC(triple: PythagoreanTriple): PythagoreanTriple = {
    val a3 = -triple.a + 2*triple.b + 2*triple.c
    val b3 = -2*triple.a + triple.b + 2*triple.c
    val c3 = -2*triple.a + 2*triple.b + 3*triple.c
    PythagoreanTriple(a3, b3, c3)
  }

}
