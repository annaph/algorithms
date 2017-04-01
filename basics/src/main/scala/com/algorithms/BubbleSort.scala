package com.algorithms

object BubbleSortMain extends App {

  import BubbleSort._

  val seq = IndexedSeq(3, 41, 52, 26, 38, 57, 9, 49)
  println("Sequence: " + seq)

  sort(seq) { _ < _ } match {
    case Some(s) =>
      println("Sorted ascending sequence: " + s)
    case None =>
      println("Empty sequence won't be sorted")
  }

}

object BubbleSort {

  def sort(s: IndexedSeq[Int])(f: (Int, Int) => Boolean): Option[IndexedSeq[Int]] =
    if (s.nonEmpty)
      Some(sort(s.toArray)(f))
    else
      None

  private def sort(ar: Array[Int])(f: (Int, Int) => Boolean): Array[Int] = {
    for {
      i <- 0 until (ar.size - 1)
      j <- (ar.size - 1) until i by -1
    } {
      if (f(ar(j), ar(j - 1))) {
        val tmp = ar(j - 1)
        ar(j - 1) = ar(j)
        ar(j) = tmp
      }
    }

    ar
  }

}
