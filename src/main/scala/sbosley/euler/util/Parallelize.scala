package sbosley.euler.util

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Parallelize {

  def apply[T, R](seq: Seq[T])(compute: Seq[T] => R): Future[Seq[R]] = {
    val cores = Runtime.getRuntime.availableProcessors
    val partitionSize = seq.length / cores
    val futures = seq.sliding(partitionSize, partitionSize).toSeq.map(partition => Future {
      compute(partition)
    })
    Future.sequence(futures)
  }

}
