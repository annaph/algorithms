package com.algorithms.digetless.password

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("""Check string "a0Ba" """) {
    val s = "a0Ba"

    val expected = 2
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""Check string "a0ba" """) {
    val s = "a0ba"

    val expected = -1
    val actual = solution(s)

    assert(actual == expected)
  }
  
  test("""Check string "a0zBaaC111111" """) {
    val s = "a0zBaaC111111"

    val expected = 4
    val actual = solution(s)

    assert(actual == expected)
  }

}
