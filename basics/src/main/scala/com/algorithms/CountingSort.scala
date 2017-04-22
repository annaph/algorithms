package com.algorithms

import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object CountingSortMain extends App {

  import CountingSort.sortAscending
  import CountingSort.sortDescending

  val seq = IndexedSeq(2, 5, 3, 0, 2, 3, 0, 3)
  println("Sequence: " + seq)

  sortAscending(seq) { a => a } match {
    case Success(v) =>
      println("Sorted ascending sequence: " + v.mkString("[", ", ", "]"))
    case Failure(e) =>
      throw new Exception(e)
  }

  sortDescending(seq) { a => a } match {
    case Success(v) =>
      println("Sorted descending sequence: " + v.mkString("[", ", ", "]"))
    case Failure(e) =>
      throw new Exception(e)
  }

}

object CountingSort {

  def sortAscending[A: ClassTag](s: IndexedSeq[A])(f: A => Int): Try[IndexedSeq[A]] =
    sort(s)(f)(asc)

  def sortDescending[A: ClassTag](s: IndexedSeq[A])(f: A => Int): Try[IndexedSeq[A]] =
    sort(s)(f)(desc)

  private def sort[A: ClassTag](s: IndexedSeq[A])(f: A => Int)(g: Array[Int] => Array[Int]): Try[IndexedSeq[A]] =
    s.size match {
      case 0 =>
        Success(IndexedSeq())
      case 1 =>
        Success(s)
      case _ =>
        val (max, ar) = maxAndMap(s)(f)
        Try {
          _sort(ar, max)(g)
        } match {
          case Success(v) =>
            Success(v)
          case Failure(e) =>
            Failure(e)
        }
    }

  private def _sort[A: ClassTag](ar: Array[(Int, A)], max: (Int, A))(f: Array[Int] => Array[Int]): Array[A] = {
    val k = max._1
    var c: Array[Int] = Array.fill(k + 1)(0)

    for {
      j <- 0 until ar.size
    } {
      val i = ar(j)._1
      c(i) = c(i) + 1
    }

    c = f(c)

    val b: Array[A] = new Array(ar.size)
    for {
      j <- (ar.size - 1) to 0 by -1
    } {
      val aj = ar(j)._1
      val cj = c(aj)

      b(cj - 1) = ar(j)._2
      c(aj) = c(aj) - 1
    }

    b
  }

  private def maxAndMap[A](s: IndexedSeq[A])(f: A => Int): ((Int, A), Array[(Int, A)]) = {
    val z: ((Int, A), Array[(Int, A)]) =
      (f(s(0)) -> s(0)) -> new Array(s.size)

    val stream: Stream[A] = Stream(s: _*)
    stream.zipWithIndex.foldLeft(z) { (acc, ai) =>
      val (max, ar) = acc
      val (a, i) = ai

      val fa = f(a)
      ar(i) = fa -> a

      if (fa > max._1)
        ar(i) -> ar
      else
        max -> ar
    }
  }

  private def asc: Array[Int] => Array[Int] =
    ar => {
      for {
        i <- 1 until ar.size
      } ar(i) = ar(i) + ar(i - 1)

      ar
    }

  private def desc: Array[Int] => Array[Int] =
    ar => {
      for {
        i <- (ar.size - 2) to 0 by -1
      } ar(i) = ar(i) + ar(i + 1)

      ar
    }

}
