package com.algorithms.balanced.parenthesis

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSpec extends FunSuite {

  test("""S: "{ [ () ] } ()" --> should be true""") {
    val s = "{ [ () ] } ()"

    val expected = true
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "{ [ ( ] ) }" --> should be false""") {
    val s = "{ [ ( ] ) }"

    val expected = false
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "   " --> should be true""") {
    val s = "   "

    val expected = true
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "[" --> should be false""") {
    val s = "["

    val expected = false
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "{()} [[" --> should be false""") {
    val s = "{()} [["

    val expected = false
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "]" --> should be false""") {
    val s = "]"

    val expected = false
    val actual = solution(s)

    assert(actual == expected)
  }

  test("""S: "({}) ]" --> should be false""") {
    val s = "({}) ]"

    val expected = false
    val actual = solution(s)

    assert(actual == expected)
  }

}
