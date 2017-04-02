package com.algorithms.favourite.number

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("X = 7; Y = 42; Array = (6,42,11,7,1,42) --> should be 4") {
    val x = 7
    val y = 42
    val a = Array(6, 42, 11, 7, 1, 42)

    val expected = 4
    val actual = solution(x, y, a)

    assert(actual == expected)
  }

  test("X = 7; Y = 42; Array = (6) --> should be 0") {
    val x = 7
    val y = 42
    val a = Array(6)

    val expected = 0
    val actual = solution(x, y, a)

    assert(actual == expected)
  }

  test("X = 7; Y = 42; Array = (7) --> should be -1") {
    val x = 7
    val y = 42
    val a = Array(7)

    val expected = -1
    val actual = solution(x, y, a)

    assert(actual == expected)
  }

  test("X = 7; Y = 7; Array = (6,42,11,7,1,42) --> should be 5") {
    val x = 7
    val y = 7
    val a = Array(6, 42, 11, 7, 1, 42)

    val expected = 5
    val actual = solution(x, y, a)

    assert(actual == expected)
  }

  test("X = 77; Y = 422; Array = (6,42,11,7,1,42) --> should be 5") {
    val x = 77
    val y = 422
    val a = Array(6, 42, 11, 7, 1, 42)

    val expected = 5
    val actual = solution(x, y, a)

    assert(actual == expected)
  }

}
