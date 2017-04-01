package com.algorithms

import scala.annotation.tailrec

object InsertionSortMain extends App {

  import InsertionSort._

  val seq = IndexedSeq(5, 2, 4, 6, 1, 3)
  println("Sequence: " + seq)

  sort(seq) { _ < _ } match {
    case Some(s) =>
      println("Sorted ascending sequence: " + s)
    case None =>
      println("Empty sequence won't be sorted")
  }

  sort(seq) { _ > _ } match {
    case Some(s) =>
      println("Sorted descending sequence: " + s)
    case None =>
      println("Empty sequence won't be sorted")
  }

}

object InsertionSort {

  def sort(s: IndexedSeq[Int])(f: (Int, Int) => Boolean): Option[IndexedSeq[Int]] =
    if (s.nonEmpty)
      Some(sort(s.toArray)(f))
    else
      None

  private def sort(ar: Array[Int])(f: (Int, Int) => Boolean): Array[Int] = {
    for {
      j <- 1 until ar.size
    } {
      @tailrec
      def loop(k: Int, key: Int): Array[Int] = k match {
        case -1 =>
          ar(0) = key
          ar
        case i if (f(ar(i), key)) =>
          ar(i + 1) = key
          ar
        case i =>
          ar(i + 1) = ar(i)
          loop(k - 1, key)
      }

      loop(j - 1, ar(j))
    }

    ar
  }

}
