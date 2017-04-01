package com.algorithms.equilibrium

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import Equilibrium.equilibrium

  def solution(a: Array[Int]): Int =
    equilibrium(a) match {
      case Success(h :: _) =>
        h
      case Success(Nil) =>
        -1
      case Failure(e) =>
        throw new Exception(e)
    }
}

object Equilibrium {

  def equilibrium(s: IndexedSeq[Int]): Try[List[Int]] =
    if (s.isEmpty)
      Success(List())
    else if (s.size == 1)
      Success(List(0))
    else
      Try(find(s)) match {
        case Success(lb) =>
          Success(lb.toList)
        case Failure(e) =>
          Failure(e)
      }

  private def find(s: IndexedSeq[Int]): ListBuffer[Int] = {
    val seq: IndexedSeq[Long] = s map { _.toLong }

    @tailrec
    def loop(k: Int, left: Long, right: Long, acc: ListBuffer[Int]): ListBuffer[Int] = {
      if (k == seq.size)
        acc
      else {
        val newRight = right - seq(k)
        if (left == newRight)
          loop(k + 1, left + seq(k), newRight, acc += k)
        else
          loop(k + 1, left + seq(k), newRight, acc)
      }
    }

    loop(0, 0L, seq.sum, ListBuffer[Int]())
  }

}
