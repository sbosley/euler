package sbosley.euler.util

object SeqExt {

  implicit class SeqExt[T](s: Seq[T]) {

    def isSorted(implicit ord: Ordering[T]): Boolean = {
      s.size <= 1 || s.sliding(2).forall { case Seq(x, y) => ord.lteq(x, y) }
    }

    def isReverseSorted(implicit ordering: Ordering[T]): Boolean = {
      isSorted(ordering.reverse)
    }
  }

}
