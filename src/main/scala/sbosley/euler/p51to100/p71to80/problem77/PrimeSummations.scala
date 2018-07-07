package sbosley.euler.p51to100.p71to80.problem77

import sbosley.euler.util.math.Primes

import scala.collection.immutable.Queue
import scala.collection.mutable

object PrimeSummations {

  private val primes = Primes.primesToMax(110).toSet

  def main(args: Array[String]): Unit = {
    // Prime partion count of 70 = 4624
    println(partitionCount(71)) // == 5006
  }

  // Based off slow solution in problem 76 with additional filtering
  type Partition = Map[Int, Int] // Represents a count of components, e.g. 3 + 3 + 2 + 2 + 2 + 1 = Map(3 -> 2, 2 -> 3, 1 -> 1)
  private def partitionCount(n: Int): Int = {
    val startPartitions = partitionInt(n)
    val seenPartitions = mutable.Set.empty[Partition]
    seenPartitions ++= startPartitions
    val queue = Queue(startPartitions.toSeq:_*)
    val allPartitions = searchPartitionTree(queue, seenPartitions).filter(_.keySet.forall(primes))
    allPartitions.size
  }

  private def partitionInt(n: Int): Set[Partition] = {
    (1 to n / 2).map(lower => {
      if (lower * 2 == n) Map(lower -> 2)
      else Map(lower -> 1, n - lower -> 1)
    })(collection.breakOut)
  }

  private def searchPartitionTree(queuedPartitions: Queue[Partition], seenPartitions: mutable.Set[Partition]): Set[Partition] = {
    if (queuedPartitions.isEmpty) seenPartitions.toSet
    else {
      val (next, remainingQueue) = queuedPartitions.dequeue
      val subpartitions = partitionChildren(next).filter(p => !p.contains(1) && !seenPartitions(p))
      seenPartitions ++= subpartitions
      searchPartitionTree(remainingQueue.enqueue(subpartitions), seenPartitions)
    }
  }

  private def partitionChildren(partition: Partition): Set[Partition] = {
    partition.flatMap[Partition, Set[Partition]] { case (elem, count) =>
      if (elem == 1) Set.empty
      else {
        (1 to elem / 2).map[Partition, Set[Partition]] { lower =>
          val upper = elem - lower
          val updatedPartition = if (count == 1) {
            partition - elem
          } else {
            partition + (elem -> (partition(elem) - 1))
          }
          if (upper == lower) updatedPartition + (upper -> (updatedPartition.getOrElse(upper, 0) + 2))
          else updatedPartition + (upper -> (updatedPartition.getOrElse(upper, 0) + 1)) + (lower -> (updatedPartition.getOrElse(lower, 0) + 1))
        }(collection.breakOut)
      }
    }(collection.breakOut)
  }

}
