package com.algorithms.seq23

import scala.annotation.tailrec

object Solution {

  def solution(n: Int): Int =
    seq23(n)

  private def seq23(n: Int): Int = {
    @tailrec
    def loop(k: Int, m: Int): Int = {
      if (isSeqNum(k))
        m match {
          case 0 =>
            k
          case _ =>
            loop(k + 1, m - 1)
        }
      else
        loop(k + 1, m)
    }

    loop(0, n)
  }

  private def isSeqNum(num: Int): Boolean = num match {
    case 0 =>
      false
    case 1 =>
      true
    case n if (n % 3 == 0) =>
      isSeqNum(n / 3)
    case n if (n % 2 == 0) =>
      isSeqNum(n / 2)
    case _ =>
      false
  }

}
