package com.algorithms

import scala.reflect.ClassTag

object MergeSortMain extends App {

  import MergeSort._

  val seq = IndexedSeq(2, 4, 5, 7, 1, 2, 3, 6)
  println("Sequence: " + seq)

  sort(seq) { _ < _ } match {
    case Some(s) =>
      println("Sorted ascending sequence: " + s)
    case None =>
      println("Empty sequence won't be sorted")
  }

}

object MergeSort {

  def sort[A: ClassTag](s: IndexedSeq[A])(f: (A, A) => Boolean): Option[IndexedSeq[A]] =
    if (s.nonEmpty) {
      Some(sort(s.toArray, 0, (s.size - 1))(f))
    } else
      None

  private def sort[A](ar: Array[A], p: Int, r: Int)(f: (A, A) => Boolean): Array[A] =
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
      var lOp: Option[A] = lIter find { _ => true }

      val rIter = right.iterator
      var rOp: Option[A] = rIter find { _ => true }

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
