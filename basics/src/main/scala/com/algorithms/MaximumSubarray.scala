package com.algorithms

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object MaximumSubarrayMain extends App {
  import MaximumSubarray._

  val seq = IndexedSeq(5, 2, 4, 6, 1, 3)
  println("Sequence: " + seq)

  find(seq) match {
    case Right(Some((low, high, sum))) =>
      println(s"low index: $low; high index: $high; maximum sum: $sum")
    case Right(None) =>
      println("Empty sequence won't be processed")
    case Left(e) =>
      println("Exception occurred: " + e.getMessage)
  }

  println("")

  val seq2 = IndexedSeq(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)
  println("Sequence 2: " + seq2)

  find(seq2) match {
    case Right(Some((low, high, sum))) =>
      println(s"low index: $low; high index: $high; maximum sum: $sum")
    case Right(None) =>
      println("Empty sequence won't be processed")
    case Left(e) =>
      println("Exception occurred: " + e.getMessage)
  }

}

object MaximumSubarray {

  def find(s: IndexedSeq[Int]): Either[Throwable, Option[(Int, Int, Int)]] =
    if (s.nonEmpty)
      (Try {
        find(s.toArray, 0, (s.length - 1))
      }) match {
        case Success(v) =>
          Right(Some(v))
        case Failure(e) =>
          Left(e)
      }
    else
      Right(None)

  private def find(ar: Array[Int], low: Int, high: Int): (Int, Int, Int) =
    if (low == high)
      (low, high, ar(low))
    else {
      val mid = (low + high) / 2

      val (ll, lh, lsum) = find(ar, low, mid)
      val (rl, rh, rsum) = find(ar, mid + 1, high)
      val (cl, ch, csum) = maxCrossing(ar, low, mid, high)

      if (lsum >= rsum && lsum >= csum)
        (ll, lh, lsum)
      else if (rsum >= lsum && rsum >= csum)
        (rl, rh, rsum)
      else
        (cl, ch, csum)
    }

  private def maxCrossing(ar: Array[Int], low: Int, mid: Int, high: Int): (Int, Int, Int) = {
    var sum = ar(mid)
    var maxLeft = mid
    var sumLeft = ar(mid)

    for {
      i <- (mid - 1) to low by -1
    } {
      sum = sum + ar(i)
      if (sum > sumLeft) {
        sumLeft = sum
        maxLeft = i
      }
    }

    sum = ar(mid + 1)
    var maxRight = mid + 1
    var sumRight = ar(mid + 1)

    for {
      j <- (mid + 2) to high
    } {
      sum = sum + ar(j)
      if (sum > sumRight) {
        sumRight = sum
        maxRight = j
      }
    }

    (maxLeft, maxRight, sumLeft + sumRight)
  }

}
