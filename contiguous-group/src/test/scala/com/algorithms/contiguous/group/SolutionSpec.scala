package com.algorithms.contiguous.group

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("A = Array(1, 2, 6, 5, 5, 8, 9) --> should be 3") {
    val a = Array(1, 2, 6, 5, 5, 8, 9)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 20, 4, 5) --> should be 3") {
    val a = Array(1, 2, 3, 20, 4, 5)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 4, 5) --> should be 0") {
    val a = Array(1, 2, 3, 4, 5)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(20, 1, 2, 3) --> should be 4") {
    val a = Array(20, 1, 2, 3)

    val expected = 4
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(20) --> should be 0") {
    val a = Array(20)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array() --> should be 0") {
    val a = Array[Int]()

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

}
