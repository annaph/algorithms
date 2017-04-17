package com.algorithms.swap.elements

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("A = Array(1, 2, 6, 5, 5, 8, 9) --> should be 1") {
    val a = Array(1, 2, 6, 5, 5, 8, 9)

    val expected = 1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 20, 5, 6, 7) --> should be 3") {
    val a = Array(1, 2, 3, 20, 5, 6, 7)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 5, 6, 7) --> should be 0") {
    val a = Array(1, 2, 3, 5, 6, 7)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(20, 1, 2, 3, 5, 6, 7) --> should be 1") {
    val a = Array(20, 1, 2, 3, 5, 6, 7)

    val expected = 6
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1) --> should be 0") {
    val a = Array(1)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(4, 2, 3, 1) --> should be 1") {
    val a = Array(4, 2, 3, 1)

    val expected = 1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(4, 3, 2, 1) --> should be 2") {
    val a = Array(4, 3, 2, 1)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

}
