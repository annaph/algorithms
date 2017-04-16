package com.algorithms

import scala.collection.mutable.HashSet
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.algorithms.RNG.random

object RandomSampleMain extends App {

  import RandomSample._

  val n = 7
  val m = 3
  println(s"m: $m; n: $n")

  sample(m, n) match {
    case Success(v) =>
      println("Random sample: " + v)
    case Failure(e) =>
      e.printStackTrace(System.out)
  }

}

object RandomSample {

  def sample(m: Int, n: Int): Try[Set[Int]] =
    if (m < 0 || n < 0 || m > n)
      Failure(new Exception("Wrong vaules for m and/or n"))
    else
      Try(_sample(m, n)) match {
        case Success(v) =>
          Success(v.toSet)
        case Failure(e) =>
          Failure(e)
      }

  private def _sample(m: Int, n: Int): HashSet[Int] =
    if (m == 0)
      HashSet()
    else {
      val s = _sample(m - 1, n - 1)
      val i = random(1, n)

      if (s contains i)
        s += n
      else
        s += i

      s
    }

}
