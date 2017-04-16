package com.algorithms

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.algorithms.MergeSort.sort
import com.algorithms.RNG.random

object PermuteBySortingMain extends App {
  import PermuteBySorting._

  val seq = IndexedSeq(1, 2, 3, 4)
  println("Sequence: " + seq)

  permute(seq) match {
    case Right(v) =>
      println(s"Permuted sequence: $v")
    case Left(e) =>
      println("Exception occurred: " + e.getMessage)
  }

}

object PermuteBySorting {

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
    val permuted: IndexedSeq[(Int, Int)] =
      for {
        i <- 0 until ar.size
      } yield {
        val r = random(1, ar.size * ar.size * ar.size)
        i -> r
      }

    sort(permuted) { _._2 < _._2 } match {
      case Some(sorted) =>
        (sorted map { a => ar(a._1) }).toArray
      case None =>
        Array()
    }
  }

}
