package com.algorithms.equilibrium

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("Array = (-1, 3, -4, 5, 1, -6, 2, 1)") {
    val a = Array(-1, 3, -4, 5, 1, -6, 2, 1)

    val expected = 1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (1, 2, 3, 4)") {
    val a = Array(1, 2, 3, 4)

    val expected = -1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (1)") {
    val a = Array(1)

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (Int.MaxValue, 5, -7, Int.MinValue, Int.MaxValue, Int.MaxValue, 1, -3, Int.MinValue)") {
    val a = Array(Int.MaxValue, 5, -7, Int.MinValue, Int.MaxValue, Int.MaxValue, 1, -3, Int.MinValue)

    val expected = 4
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = ()") {
    val a: Array[Int] = Array()

    val expected = -1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue)") {
    val a = Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue, Int.MinValue, 3, 0)") {
    val a = Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue, Int.MinValue, 3, 0)

    val expected = 7
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Array = (Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue, Int.MaxValue)") {
    val a = Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue, Int.MaxValue)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

}
