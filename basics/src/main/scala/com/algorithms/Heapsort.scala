package com.algorithms

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object HeapsortMain extends App {

  import Heapsort.sort

  val seq = IndexedSeq(16, 14, 10, 8, 7, 9, 3, 2, 4, 1)
  println("Sequence: " + seq)

  sort(seq) { _ < _ } match {
    case Success(v) =>
      println("Sorted ascending sequence: " + v.mkString("[", ", ", "]"))
    case Failure(e) =>
      throw new Exception(e)
  }

}

object Heapsort {

  def sort(s: IndexedSeq[Int])(f: (Int, Int) => Boolean): Try[IndexedSeq[Int]] =
    s.size match {
      case x if (x == 0 || x == 1) =>
        Success(s)
      case _ =>
        Try { _sort(s.toArray)(f) } match {
          case Success(v) =>
            Success(v)
          case Failure(e) =>
            Failure(e)
        }
    }

  private def _sort(ar: Array[Int])(f: (Int, Int) => Boolean): Array[Int] = {
    val g: (Int, Int) => Boolean = { (x, y) => !f(x, y) }
    var heap = buildHeap(ar)(g)
    var heapSize = ar.size

    for {
      i <- (ar.size - 1) until 0 by -1
    } {
      val tmp = heap(0)
      heap(0) = heap(i)
      heap(i) = tmp

      heapSize = heapSize - 1
      heap = heapify(heap, 0, heapSize)(g)
    }

    heap
  }

  private def buildHeap(ar: Array[Int])(f: (Int, Int) => Boolean): Array[Int] = {
    var a = ar
    val half = a.size / 2

    for {
      i <- (half - 1) to 0 by -1
    } a = heapify(a, i, a.size)(f)

    a
  }

  @tailrec
  private def heapify(ar: Array[Int], i: Int, heapSize: Int)(f: (Int, Int) => Boolean): Array[Int] = {
    val l = left(i)
    val r = right(i)
    var largest = i

    if (l < heapSize && f(ar(l), ar(i)))
      largest = l
    if (r < heapSize && f(ar(r), ar(largest)))
      largest = r

    if (largest == i)
      ar
    else {
      val tmp = ar(i)
      ar(i) = ar(largest)
      ar(largest) = tmp

      heapify(ar, largest, heapSize)(f)
    }
  }

  private def left(i: Int): Int =
    (2 * i) + 1

  private def right(i: Int): Int =
    (2 * i) + 2

}
