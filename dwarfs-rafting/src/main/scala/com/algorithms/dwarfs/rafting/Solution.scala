package com.algorithms.dwarfs.rafting

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {
  import DwarfsRafting.dwarfs

  def solution(n: Int, s: String, t: String): Int =
    Try { dwarfs(n, s, t) } match {
      case Success(Some((a, b, c, d))) =>
        a + b + c + d
      case Success(None) =>
        -1
      case Failure(e) =>
        throw new Exception(e)
    }

}

object DwarfsRafting {
  case class Partition(occupiedSeats: Int, freeSeats: Int)

  type Combination = (Int, Int, Int, Int)
  type Position = (Int, Int)
  type Positions = IndexedSeq[Position]
  type Raft = (Partition, Partition, Partition, Partition)

  def dwarfs(n: Int, s: String, t: String): Option[Combination] = n match {
    case 0 =>
      None
    case _ =>
      combination(n, stringToPositions(s), stringToPositions(t))
  }

  private def combination(n: Int, s: Positions, t: Positions): Option[Combination] = {
    val f: Position => Partition = { partitionRaft(n, _, s, t) }
    val g: Raft => Option[Combination] = allCombinations _ andThen bestCombination _

    val partitionA = f(0 -> 0)
    val partitionB = f(0 -> (n / 2))
    val partitionC = f((n / 2) -> 0)
    val partitionD = f((n / 2) -> (n / 2))

    g((partitionA, partitionB, partitionC, partitionD))
  }

  private def partitionRaft(n: Int, topLeft: Position, s: Positions, t: Positions): Partition = {
    val top = topLeft._1
    val left = topLeft._2

    def count(as: Positions): Int = as.foldLeft(0) { (acc, p) =>
      val (r, c) = p
      if ((r >= top && r < (top + n / 2)) &&
        (c >= left && c < (left + n / 2)))
        acc + 1
      else
        acc
    }

    val total = (n * n) / 4
    val barrels = count(s)
    val occupied = count(t)

    Partition(occupied, total - barrels - occupied)
  }

  private def allCombinations(r: Raft): (Raft, List[Combination]) =
    r -> (for {
      a <- 0 to r._1.freeSeats
      b <- 0 to r._2.freeSeats
      c <- 0 to r._3.freeSeats
      d <- 0 to r._4.freeSeats
    } yield (a, b, c, d)).toList

  private def bestCombination(rl: (Raft, List[Combination])): Option[Combination] = {
    val (r, as) = rl

    val ca = r._1.occupiedSeats
    val cb = r._2.occupiedSeats
    val cc = r._3.occupiedSeats
    val cd = r._4.occupiedSeats

    val validCombinations: List[Combination] = as filter { comb =>
      val (a, b, c, d) = comb

      val q1 = a + ca
      val q2 = b + cb
      val q3 = c + cc
      val q4 = d + cd

      if ((q1 + q2) == (q3 + q4) &&
        (q1 + q3) == (q2 + q4))
        true
      else
        false
    }

    (validCombinations.toStream map { c =>
      (c._1 + c._2 + c._3 + c._4) -> c
    }).foldLeft(None: Option[(Int, Combination)]) { (acc, c) =>
      acc match {
        case Some(v) =>
          if (c._1 > v._1)
            Some(c)
          else
            acc
        case None =>
          Some(c)
      }
    } map { _._2 }
  }

  private def stringToPositions(s: String): Positions =
    s.split(' ') filter { _.length != 0 } map { p =>
      p(0) -> p(1)
    } map {
      case (r, c) =>
        val row = r.asDigit - 1
        val column = c - 'A'

        row -> column
    }

}
