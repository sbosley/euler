package sbosley.euler.p1to20.problem19

import java.time.{DayOfWeek, LocalDate}

object Problem19 {

  def main(args: Array[String]): Unit = {
    val sundayCount = Stream.iterate(LocalDate.of(1901, 1, 1))(_.plusMonths(1))
      .takeWhile(_.getYear <= 2000)
      .count(_.getDayOfWeek == DayOfWeek.SUNDAY)
    println(sundayCount)
  }

}
