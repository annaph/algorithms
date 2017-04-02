package com.algorithms.favourite.number

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import FavouriteNumber.prefix

  def solution(x: Int, y: Int, a: Array[Int]): Int =
    prefix(a, x, y) match {
      case Success(v) =>
        v
      case Failure(e) =>
        throw new Exception(e)
    }

}

object FavouriteNumber {

  type Result = (Int, Int)

  def prefix(s: IndexedSeq[Int], x: Int, y: Int): Try[Int] = (x, y) match {
    case (a, b) if (a == b) =>
      Success(s.size - 1)
    case _ =>
      Try { _prefix(s, x, y) } match {
        case Success((n, i)) =>
          Success(i)
        case Failure(e) =>
          Failure(e)
      }
  }

  private def _prefix(s: IndexedSeq[Int], x: Int, y: Int): Result = {
    var numOfX = 0
    var numOfY = 0
    var occ = 0
    val stream = Stream(s: _*)

    val index: Int = stream.zipWithIndex.foldLeft(-1) { (acc, a) =>
      if (x == a._1)
        numOfX = numOfX + 1
      else if (y == a._1)
        numOfY = numOfY + 1

      if (numOfX == numOfY) {
        occ = numOfX
        a._2
      } else
        acc
    }

    occ -> index
  }

}
