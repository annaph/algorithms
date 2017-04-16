package com.algorithms

import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import RNG.random

object QuicksortMain extends App {

  import Quicksort.sort

  val seq = IndexedSeq(2, 8, 7, 5, 1, 5, 5, 3, 5, 6, 4)
  println("Sequence: " + seq)

  sort(seq) { _ < _ } match {
    case Success(v) =>
      println("Sorted ascending sequence: " + v.mkString("[", ", ", "]"))
    case Failure(e) =>
      throw new Exception(e)
  }

}

object Quicksort {

  def sort[A: ClassTag](s: IndexedSeq[A])(f: (A, A) => Boolean)(implicit ord: Ordering[A]): Try[IndexedSeq[A]] = s.size match {
    case n if (n == 0 || n == 1) =>
      Success(s)
    case 2 =>
      if (f(s(0), s(1)))
        Success(s)
      else
        Success(IndexedSeq(s(1), s(0)))
    case _ =>
      Try { _sort(s.toArray, 0, s.size - 1)(f)(ord) } match {
        case Success(v) =>
          Success(v)
        case Failure(e) =>
          Failure(e)
      }
  }

  private def _sort[A](ar: Array[A], p: Int, r: Int)(f: (A, A) => Boolean)(ord: Ordering[A]): Array[A] =
    if (p >= r)
      ar
    else {
      var (q, t, a) = randomizedPartition(ar, p, r)(f)(ord)
      a = _sort(a, p, q - 1)(f)(ord)
      a = _sort(a, t + 1, r)(f)(ord)

      a
    }

  private def randomizedPartition[A](ar: Array[A], p: Int, r: Int)(f: (A, A) => Boolean)(ord: Ordering[A]): (Int, Int, Array[A]) = {
    val i = random(p, r)
    val a = swap(ar, i, r)

    partition(a, p, r)(f)(ord)
  }

  private def partition[A](ar: Array[A], p: Int, r: Int)(f: (A, A) => Boolean)(ord: Ordering[A]): (Int, Int, Array[A]) = {
    var a = ar
    val x = a(r)
    var q, t = p - 1

    for {
      j <- p to (r - 1)
    } if (ord equiv (a(j), x)) {
      t = t + 1
      a = swap(a, t, j)
    } else if (f(ar(j), x)) {
      q = q + 1
      t = t + 1

      a = swap(a, q, j)
      if (t != q)
        a = swap(a, t, j)
    }

    t = t + 1
    a = swap(a, t, r)

    (q, t, a)
  }

  private def swap[A](ar: Array[A], i: Int, j: Int): Array[A] = {
    val tmp = ar(i)
    ar(i) = ar(j)
    ar(j) = tmp

    ar
  }

}
