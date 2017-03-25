package com.algorithms.tree.path

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import Solution.solution

@RunWith(classOf[JUnitRunner])
class SolutionSuite extends FunSuite {

  test("Find most distinct path for empty tree") {
    val a: Tree = null

    val expected = 0
    val actual = solution(a)

    assert(actual == expected)
  }

  test("Find most distinct path for non-empty tree") {
    val g = new Tree(5, null, null)
    val d = new Tree(4, g, null)
    val b = new Tree(5, d, null)
    val e = new Tree(1, null, null)
    val f = new Tree(6, null, null)
    val c = new Tree(6, e, f)
    val a = new Tree(4, b, c)

    val expected = 3
    val actual = solution(a)

    assert(actual == expected)
  }

}
