package com.algorithms

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.algorithms.RNG.choose

object RandomizeInPlaceMain extends App {

  import RandomizeInPlace._

  val seq = IndexedSeq(1, 2, 3, 4)
  println("Sequence: " + seq)

  permute(seq) match {
    case Right(v) =>
      println(s"Permuted sequence: $v")
    case Left(e) =>
      println("Exception occurred: " + e.getMessage)
      e.printStackTrace(System.out)
  }

}

object RandomizeInPlace {

  def permute(s: IndexedSeq[Int]): Either[Throwable, IndexedSeq[Int]] = {
    if (s.nonEmpty) {
      (Try {
        permute(s.toArray)
      }) match {
        case Success(v) =>
          Right(v)
        case Failure(e) =>
          Left(e)
      }
    } else
      Right(s)
  }

  private def permute(ar: Array[Int]): Array[Int] = {
    var permuted = ar
    for {
      i <- 0 until ar.size
    } {
      val r = choose(i, ar.size - 1)
      permuted = swap(ar, i, r)
    }

    permuted
  }

  private def swap(ar: Array[Int], i1: Int, i2: Int): Array[Int] = {
    val tmp = ar(i1)
    ar(i1) = ar(i2)
    ar(i2) = tmp

    ar
  }

}
