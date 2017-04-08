package com.algorithms.linked.list

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("A = Array(1,4,-1,3,2) --> should be 4") {
    val a = Array(1, 4, -1, 3, 2)

    val expected = 4
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(-1) --> should be 1") {
    val a = Array(-1)

    val expected = 1
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(1, -1) --> should be 2") {
    val a = Array(1, -1)

    val expected = 2
    val actual = solution(a)

    assert(actual == expected)
  }

  test("A = Array(-1, 4) --> should be 1") {
    val a = Array(-1, 4)

    val expected = 1
    val actual = solution(a)

    assert(actual == expected)
  }

}
