package sbosley.euler.string

import scala.annotation.tailrec

object StringExt {

  implicit class StringExt(s: String) {

    @tailrec
    final def isPalindrome: Boolean = {
      if (s.isEmpty || s.length == 1) true
      else if (s.charAt(0) != s.charAt(s.length - 1)) false
      else s.substring(1, s.length - 1).isPalindrome
    }

    def isPermutation(other: String): Boolean = {
      s.sorted == other.sorted
    }

    def rotate(n: Int): String = {
      s.substring(n) + s.substring(0, n)
    }
  }
}
