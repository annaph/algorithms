package com.algorithms.swap.elements

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import SwapElements.swap

  def solution(a: Array[Int]): Int =
    swap(a) match {
      case Success(v) =>
        v.length
      case Failure(e) =>
        throw new Exception(e)
    }

}

object SwapElements {

  type Diff = (Int, (Int, Int))
  type Swap = (Int, Int)
  type Result = ListBuffer[Swap]

  def swap(s: IndexedSeq[Int]): Try[List[Swap]] = s.size match {
    case 1 =>
      Success(Nil)
    case _ =>
      Try { _swap(s.toArray) } match {
        case Success(v) =>
          Success(v.toList)
        case Failure(e) =>
          Failure(e)
      }
  }

  private def _swap(ar: Array[Int]): Result = {
    val sorted = sort(ar.clone(), 0, (ar.size - 1)) { _ < _ }

    val diffs: HashMap[Int, (Int, Int)] = HashMap()
    for {
      i <- 0 until ar.size
    } {
      if (ar(i) != sorted(i))
        diffs += ar(i) -> (sorted(i), i)
    }

    @tailrec
    def go(m: HashMap[Int, (Int, Int)], acc: Result): Result = m.isEmpty match {
      case true =>
        acc
      case false =>
        val h: Diff = m.head
        m get (h._2._1) match {
          case Some(v) =>
            val (s, d) = doSwap(h, (h._2._1) -> v)

            m -= h._1
            m -= h._2._1

            if (d._1 != d._2._1)
              m += d

            go(m, acc += s)
          case None =>
            throw new Error("error")
        }
    }

    go(diffs, ListBuffer[Swap]())
  }

  private def doSwap(d1: Diff, d2: Diff): (Swap, Diff) = {
    val d: Diff = d1._1 -> (d2._2._1, d2._2._2)
    val s: Swap = d1._1 -> d2._1

    s -> d
  }

  private def sort(ar: Array[Int], p: Int, r: Int)(f: (Int, Int) => Boolean): Array[Int] =
    if (p == r)
      ar
    else {
      val q = ((r - p) / 2) + p

      sort(ar, p, q)(f)
      sort(ar, q + 1, r)(f)

      val n1 = q - p + 1
      val n2 = r - q

      val left = ar slice (p, p + n1)
      val right = ar slice (q + 1, q + 1 + n2)

      val lIter = left.iterator
      var lOp: Option[Int] = lIter find { _ => true }

      val rIter = right.iterator
      var rOp: Option[Int] = rIter find { _ => true }

      for {
        k <- p to r
      } {
        ((lOp, rOp): @unchecked) match {
          case (Some(x), Some(y)) =>
            if (f(x, y)) {
              ar(k) = x
              lOp = lIter find { _ => true }
            } else {
              ar(k) = y
              rOp = rIter find { _ => true }
            }
          case (Some(x), None) =>
            ar(k) = x
            lOp = lIter find { _ => true }
          case (None, Some(y)) =>
            ar(k) = y
            rOp = rIter find { _ => true }
        }
      }

      ar
    }

}
