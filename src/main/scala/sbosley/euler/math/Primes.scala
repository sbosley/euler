package sbosley.euler.math

object Primes {

  def primes[T : Integral](max: T): Seq[T] = {
    val integral = implicitly[Integral[T]]
    val two = integral.plus(integral.one, integral.one)
    seive(Stream.iterate(two) { a => integral.plus(a, integral.one) }).takeWhile(a => integral.lt(a, max))
  }

  private def seive[T : Integral](stream: Stream[T]): Stream[T] = {
    val head = stream.head
    head #:: seive(stream.tail.filter(implicitly[Integral[T]].rem(_, head) != 0))
  }

}
