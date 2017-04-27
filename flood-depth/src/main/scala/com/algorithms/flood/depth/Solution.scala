package com.algorithms.flood.depth

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.max
import scala.math.min
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import FloodDepth.depths

  def solution(a: Array[Int]): Int =
    depths(a) match {
      case Success(Nil) =>
        0
      case Success(v) =>
        v.max
      case Failure(e) =>
        throw new Exception(e)
    }

}

object FloodDepth {
  type L = Option[Int]
  type M = Option[Int]
  type R = Option[Int]
  type D = Int
  type U = (L, M, R, D)
  type Result = List[U]

  def depths(s: IndexedSeq[Int]): Try[List[Int]] = s.size match {
    case x if (x == 0 || x == 1) =>
      Success(Nil)
    case _ =>
      Try { _depths(s.toArray) } match {
        case Success(v) =>
          val l: List[Int] = v map {
            case (_, _, _, depth) =>
              depth
          }
          Success(l)
        case Failure(e) =>
          Failure(e)
      }
  }

  private def _depths(ar: Array[Int]): Result = {
    @tailrec
    def loop(k: Int, u: U, acc: ListBuffer[U]): ListBuffer[U] =
      if (k == ar.size)
        acc += u
      else {
        val (d, newU) = nextU(ar(k))(u)
        d match {
          case Some(v) =>
            loop(k + 1, newU, acc += v)
          case None =>
            loop(k + 1, newU, acc)
        }
      }

    loop(0, (None, None, None, 0), ListBuffer()).toList
  }

  private def nextU(n: Int)(s: U): (Option[U], U) = s match {
    case (Some(l), None, None, _) =>
      leftKnown(n, l)(s)
    case (Some(l), Some(m), None, _) =>
      leftMiddletKnown(n, l, m)(s)
    case (Some(l), Some(m), Some(r), _) =>
      leftMiddleRightKnown(n, l, m, r)(s)
    case _ =>
      None -> (Some(n), None, None, 0)
  }

  private def leftKnown(n: Int, l: Int)(u: U): (Option[U], U) =
    if (n < l)
      None -> (Some(l), Some(n), None, 0)
    else if (n == l)
      None -> u
    else {
      val v = (None, Some(l), Some(n), 0)
      Some(v) -> (Some(n), None, None, 0)
    }

  private def leftMiddletKnown(n: Int, l: Int, m: Int)(u: U): (Option[U], U) =
    if (n < m)
      None -> (Some(l), Some(n), None, u._4)
    else if (n == m)
      None -> u
    else
      None -> (Some(l), Some(m), Some(n), newDepth(l, m, n, u._4))

  private def leftMiddleRightKnown(n: Int, l: Int, m: Int, r: Int)(u: U): (Option[U], U) =
    if (n < r && n < m)
      None -> (Some(l), Some(n), None, u._4)
    else if (n < r || n == r)
      None -> u
    else if (n > r && n < l)
      None -> (Some(l), Some(m), Some(n), newDepth(l, m, n, u._4))
    else {
      val v = (Some(l), Some(m), Some(n), newDepth(l, m, n, u._4))
      Some(v) -> (Some(n), None, None, 0)
    }

  private def newDepth(l: Int, m: Int, r: Int, d: Int): Int = {
    val depth = min(l, r) - m
    max(d, depth)
  }

}
