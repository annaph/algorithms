package com.algorithms.contiguous.group

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {
  import ContiguousGroup.group

  def solution(a: Array[Int]): Int =
    Try { group(a) } match {
      case Success(v) =>
        v match {
          case Some((s, e)) =>
            e - s + 1
          case None =>
            0
        }
      case Failure(e) =>
        throw new Exception(e)
    }

}

object ContiguousGroup {
  type Element = (Int, Int)
  type Group = Option[Element]

  def group(s: IndexedSeq[Int]): Group = s.size match {
    case x if (x == 0 || x == 1) =>
      None
    case _ =>
      _group(s.toArray)
  }

  private def _group(ar: Array[Int]): Group = {
    val sorted: Array[Int] = sort(ar.clone, 0, (ar.size - 1)) { _ < _ }

    for {
      s <- start(ar, sorted)
      e <- end(ar, sorted)
    } yield s._1 -> e._1
  }

  private def start(ar: Array[Int], s: Array[Int]): Option[Element] =
    firstNonMatching(ar, s)(0) { _ + 1 }

  private def end(ar: Array[Int], s: Array[Int]): Option[Element] =
    firstNonMatching(ar, s)(ar.size - 1) { _ - 1 }

  private def firstNonMatching(ar: Array[Int], s: Array[Int])(z: Int)(f: Int => Int): Option[Element] = {
    lazy val stream: Stream[Int] = z #:: (stream map f)

    stream map { i => i -> ar(i) } take ar.size find {
      case (i, v) =>
        if (s(i) != v)
          true
        else
          false
    }
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
