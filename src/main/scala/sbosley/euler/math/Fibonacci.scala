package sbosley.euler.math

object Fibonacci {

  val stream: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: stream.zip(stream.tail).map { n => n._1 + n._2 }

}
