package com.algorithms.sum.pairs

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import SumPairs.pairs

  def solution(a: Array[Int], m: Int): Int =
    pairs(a, m) match {
      case Success(v) =>
        v.length
      case Failure(e) =>
        throw new Exception(e)
    }

}

object SumPairs {

  type Pair = (Int, Int)
  type Pairs = ListBuffer[Pair]
  type State = HashMap[Int, (Int, Int)]

  def pairs(s: IndexedSeq[Int], m: Int): Try[List[Pair]] =
    if (s.size == 1)
      Success(Nil)
    else
      Try { _pairs(s, m) } match {
        case Success(v) =>
          Success(v.toList)
        case Failure(e) =>
          Failure(e)
      }

  private def _pairs(seq: IndexedSeq[Int], m: Int): ListBuffer[Pair] = {
    var state: State = HashMap()

    (Stream(seq: _*) map { a => a -> (m - a) }).foldLeft(ListBuffer[Pair]()) { (acc, t) =>
      val (p, s) = nextPair(t._1, t._2)(state)
      state = s

      p match {
        case Some(v) =>
          acc += v
        case None =>
          acc
      }
    }
  }

  private def nextPair(v1: Int, v2: Int)(state: State): (Option[Pair], State) =
    (state get v2) match {
      case Some((v, 1)) if (v1 == v) =>
        state -= v2
        (Some(v2 -> v1), state)
      case Some((v, i)) if (v1 == v) =>
        state += (v2 -> (v1, i - 1))
        (Some(v2 -> v1), state)
      case _ =>
        state get v1 match {
          case Some((_, i)) =>
            state += (v1 -> (v2, i + 1))
            None -> state
          case _ =>
            state += (v1 -> (v2, 1))
            None -> state
        }
    }

}
