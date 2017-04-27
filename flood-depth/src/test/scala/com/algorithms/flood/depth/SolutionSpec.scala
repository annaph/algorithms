package com.algorithms.flood.depth

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("A = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2) --> should be 2") {
    val a = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(5, 8) --> should be 0") {
    val a = Array(5, 8)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(7) --> should be 0") {
    val a = Array(7)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2) --> should be 2") {
    val a = Array(1, 1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 3, 2, 1, 2, 2, 1, 5, 3, 3, 4, 2) --> should be 2") {
    val a = Array(1, 3, 2, 1, 2, 2, 1, 5, 3, 3, 4, 2)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2, 7) --> should be 3") {
    val a = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2, 7)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2, 4) --> should be 2") {
    val a = Array(1, 3, 2, 1, 2, 1, 5, 3, 3, 4, 2, 4)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

}
