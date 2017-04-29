package com.algorithms.dwarfs.rafting

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("N even --> should be 6") {
    val n = 4
    val s = "1B 1C 4B 1D 2A"
    val t = "3B 2D"

    val expected = 6
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

  test("No barrels and occupied seats --> should be 16") {
    val n = 4
    val s = ""
    val t = ""

    val expected = 16
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

  test("No free seats but balanced --> should be 0") {
    val n = 4
    val s = "1A 1B 2A 1C 1D 2C 3A 3B 4A 3C 3D 4C"
    val t = "2B 2D 4B 4D"

    val expected = 0
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

  test("No free seats and not balanced --> should be -1") {
    val n = 4
    val s = "1A 1B 2A 1C 1D 3A 3B 4A 3C 3D 4C"
    val t = "2B 2D 4B 4D 2C"

    val expected = -1
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

  test("N = 2 --> should be 4") {
    val n = 2
    val s = ""
    val t = ""

    val expected = 4
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

  test("N = 0 --> should be -1") {
    val n = 0
    val s = ""
    val t = ""

    val expected = -1
    val actual = solution(n, s, t)

    assert(actual == expected)
  }

}
