package com.algorithms.seq23

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("N = 0") {
    val n = 0

    val expected = 1
    val actual = solution(n)

    assert(actual == expected)
  }

  test("N = 4") {
    val n = 4

    val expected = 6
    val actual = solution(n)

    assert(actual == expected)
  }

  test("N = 7") {
    val n = 7

    val expected = 12
    val actual = solution(n)

    assert(actual == expected)
  }

  test("N = 17") {
    val n = 17

    val expected = 72
    val actual = solution(n)

    assert(actual == expected)
  }

}
