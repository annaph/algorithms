package com.algorithms.sum.pairs

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpect extends FunSuite {

  test("A = Array(2, 7, 1, 3, 2, 4, -3); M = 4 --> should be 3") {
    val a = Array(2, 7, 1, 3, 2, 4, -3)
    val m = 4

    val expected = 3
    val actual = solution(a, m)

    assert(actual == expected)
  }

  test("A = Array(2, 2, 2); M = 4 --> should be 1") {
    val a = Array(2, 2, 2)
    val m = 4

    val expected = 1
    val actual = solution(a, m)

    assert(actual == expected)
  }

  test("A = Array(2); M = 2 --> should be 0") {
    val a = Array(2)
    val m = 2

    val expected = 0
    val actual = solution(a, m)

    assert(actual == expected)
  }

  test("A = Array(2, 2, 2, 2); M = 4 --> should be 2") {
    val a = Array(2, 2, 2, 2)
    val m = 4

    val expected = 2
    val actual = solution(a, m)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 4); M = 100 --> should be 0") {
    val a = Array(1, 2, 3, 4)
    val m = 100

    val expected = 0
    val actual = solution(a, m)

    assert(actual == expected)
  }

  test("A = Array(7, 3, 3, 1, 1): M = 4 --> should be 2") {
    val a = Array(7, 3, 3, 1, 1)
    val m = 4

    val expected = 2
    val actual = solution(a, m)

    assert(actual == expected)
  }

}
