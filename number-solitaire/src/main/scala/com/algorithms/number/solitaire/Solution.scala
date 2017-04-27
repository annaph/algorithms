package com.algorithms.number.solitaire

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import NumberSolitaire.path

  def solution(a: Array[Int]): Int =
    path(a) match {
      case Success(v) =>
        v.foldLeft(0) { _ + _._2 }
      case Failure(e) =>
        throw new Exception(e)
    }

}

object NumberSolitaire {

  type Point = (Int, Int)
  type Path = List[Point]

  def path(s: IndexedSeq[Int]): Try[Path] = s.size match {
    case 0 =>
      Success(Nil)
    case 1 =>
      Success(List(0 -> s(0)))
    case 2 =>
      Success(List(
        0 -> s(0),
        1 -> s(1)))
    case _ =>
      Try { _path(s.toArray) } match {
        case x @ Success(_) =>
          x
        case e @ Failure(_) =>
          e
      }
  }

  private def _path(ar: Array[Int]): Path = {
    @tailrec
    def go(k: Int, acc: ListBuffer[Point]): ListBuffer[Point] =
      if (k == ar.size)
        acc
      else {
        val (p, r) = nextPoint(ar)(k)
        go(r, acc += p)
      }

    go(1, ListBuffer(0 -> ar(0))).toList
  }

  private def nextPoint(ar: Array[Int])(r: Int): (Point, Int) = {
    val n = negatives(ar, r)

    val i = r + n.length
    val cr = i -> ar(i)

    handleNegatives(n, cr) match {
      case None =>
        cr -> (i + 1)
      case Some(p) =>
        p -> (p._1 + 1)
    }
  }

  private def negatives(ar: Array[Int], r: Int): ListBuffer[Point] = {
    @tailrec
    def loop(k: Int, acc: ListBuffer[Point]): ListBuffer[Point] =
      if (k == (ar.size - 1) ||
        ar(k) >= 0 ||
        k == (r + 12))
        acc
      else
        loop(k + 1, acc += (k -> ar(k)))

    loop(r, ListBuffer())
  }

  private def handleNegatives(n: ListBuffer[Point], cr: Point): Option[Point] = n.length match {
    case s if (s < 6) =>
      None
    case s if (s == 6) =>
      Some(maxPoint(n))
    case _ =>
      Some(bestCandidate(n, cr))
  }

  private def bestCandidate(n: ListBuffer[Point], cr: Point): Point = {
    val n6 = n take 6

    val l: ListBuffer[Point] = n6 map { p =>
      val bestAhead = maxAhead(p, n, cr)
      p._1 -> (p._2 + bestAhead._2)
    }

    val bestIndex: Int = maxPoint(l)._1

    (n find { p =>
      if (p._1 == bestIndex)
        true
      else
        false
    }).get
  }

  private def maxAhead(p: Point, n: ListBuffer[Point], cr: Point): Point = {
    val sixAhead = (n drop (p._1 - n.head._1 + 1)) take 6

    if (sixAhead.length < 6)
      cr._1 -> Int.MaxValue
    else
      maxPoint(sixAhead)
  }

  private def maxPoint(l: ListBuffer[Point]): Point =
    l.tail.foldLeft(l.head) { (acc, p) =>
      if (p._2 >= acc._2)
        p
      else
        acc
    }

}
