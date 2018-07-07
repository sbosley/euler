package sbosley.euler.p51to100.p71to80.problem76

import sbosley.euler.util.math.Primes

import scala.collection.mutable

object CountingSummations {


  // https://en.wikipedia.org/wiki/Partition_(number_theory)#Other_recurrence_relations
  // p(n) = (1 / n) * sum from k=0 to n-1 of sum_of_divisors(n-k) * p(k)

  private val memoizedPartitions = mutable.Map[Int, BigInt](0 -> 1, 1 -> 1)
  def main(args: Array[String]): Unit = {
    println(partition(100) - 1)
  }

  private val primes = Primes.primesToMax(110)

  private def partition(n: Int): BigInt = {
    memoizedPartitions.getOrElseUpdate(n, {
      (0 until n).map(k => {
        Primes.allDivisors(n - k, Some(primes)).sum * partition(k)
      }).sum / n
    })
  }

//  // Slow solution that enumerates partitions
//  type Partition = Map[Int, Int] // Represents a count of components, e.g. 3 + 3 + 2 + 2 + 2 + 1 = Map(3 -> 2, 2 -> 3, 1 -> 1)
//  private def partitionCount(n: Int): Int = {
//    val startPartitions = partitionInt(n)
//    val seenPartitions = mutable.Set.empty[Partition]
//    seenPartitions ++= startPartitions
//    val queue = Queue(startPartitions.toSeq:_*)
//    val allPartitions = searchPartitionTree(queue, seenPartitions)
//    allPartitions.size + 1
//  }
//
//  private def partitionInt(n: Int): Set[Partition] = {
//    (1 to n / 2).map(lower => {
//      if (lower * 2 == n) Map(lower -> 2)
//      else Map(lower -> 1, n - lower -> 1)
//    })(collection.breakOut)
//  }
//
//  private def searchPartitionTree(queuedPartitions: Queue[Partition], seenPartitions: mutable.Set[Partition]): Set[Partition] = {
//    if (queuedPartitions.isEmpty) seenPartitions.toSet
//    else {
//      val (next, remainingQueue) = queuedPartitions.dequeue
//      val subpartitions = partitionChildren(next).filterNot(seenPartitions)
//      seenPartitions ++= subpartitions
//      searchPartitionTree(remainingQueue.enqueue(subpartitions), seenPartitions)
//    }
//  }
//
//  private def partitionChildren(partition: Partition): Set[Partition] = {
//    partition.flatMap[Partition, Set[Partition]] { case (elem, count) =>
//      if (elem == 1) Set.empty
//      else {
//        (1 to elem / 2).map[Partition, Set[Partition]] { lower =>
//          val upper = elem - lower
//          val updatedPartition = if (count == 1) {
//            partition - elem
//          } else {
//            partition + (elem -> (partition(elem) - 1))
//          }
//          if (upper == lower) updatedPartition + (upper -> (updatedPartition.getOrElse(upper, 0) + 2))
//          else updatedPartition + (upper -> (updatedPartition.getOrElse(upper, 0) + 1)) + (lower -> (updatedPartition.getOrElse(lower, 0) + 1))
//        }(collection.breakOut)
//      }
//    }(collection.breakOut)
//  }

}
