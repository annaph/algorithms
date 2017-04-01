package com.algorithms

import scala.math.abs

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  private case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  private var currState: RNG = SimpleRNG(12)

  def choose(a: Int, b: Int): Int = {
    val (i, s) = currState.nextInt
    currState = s

    (abs(i) % (b - a + 1)) + a
  }

}
