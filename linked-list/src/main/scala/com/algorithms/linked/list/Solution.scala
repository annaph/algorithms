package com.algorithms.linked.list

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import LinkedList.toList

  def solution(a: Array[Int]): Int =
    toList(a) match {
      case Success(v) =>
        v.length
      case Failure(e) =>
        throw new Exception(e)
    }

}

object LinkedList {

  def toList(s: IndexedSeq[Int]): Try[List[Int]] =
    if (s(0) == -1)
      Success(List(-1))
    else
      Try { _toList(s) } match {
        case Success(v) =>
          println(v)
          Success(v.toList)
        case Failure(e) =>
          Failure(e)
      }

  private def _toList(s: IndexedSeq[Int]): ListBuffer[Int] = {
    @tailrec
    def loop(k: Int, acc: ListBuffer[Int]): ListBuffer[Int] = s(s(k)) match {
      case -1 =>
        acc += -1
      case v =>
        loop(s(k), acc += v)
    }

    loop(0, ListBuffer(s(0)))
  }

}
