package com.algorithms.number.solitaire

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("A = Array(1, -2, 0, 9, -1, -2) --> should be 8") {
    val a = Array(1, -2, 0, 9, -1, -2)

    val expected = 8
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(2, -1, -100, -200, -300, -400, -1, -1, -10, -20, 7) --> should be 8") {
    val a = Array(2, -1, -100, -200, -300, -400, -1, -1, -10, -20, 7)

    val expected = 8
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(2, -1, -100, -200, -300, -400, -1, -1, -10, -20, -7) --> should be -6") {
    val a = Array(2, -1, -100, -200, -300, -400, -1, -1, -10, -20, -7)

    val expected = -6
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1, 7) --> should be 24") {
    val a = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1, 7)

    val expected = 24
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1, -7) --> should be 10") {
    val a = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1, -7)

    val expected = 10
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1) --> should be 17") {
    val a = Array(19, -1, -100, -200, -300, -400, -1, -10, -20, -30, -40, -50, -1)

    val expected = 17
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(2, -1, -100, -200, -300, 0, -1, -1, -10, -20, 7) --> should be 9") {
    val a = Array(2, -1, -100, -200, -300, 0, -1, -1, -10, -20, 7)

    val expected = 9
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 2, 3, 4, 5) --> should be 15") {
    val a = Array(1, 2, 3, 4, 5)

    val expected = 15
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, 17) --> should be 18") {
    val a = Array(1, 17)

    val expected = 18
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(12) --> should be 12") {
    val a = Array(12)

    val expected = 12
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array() --> should be 0") {
    val a = Array[Int]()

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17) --> should be 27") {
    val a = Array(12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17)

    val expected = 27
    val actual = solution(a)

    assert(actual == expected)
  }

}
