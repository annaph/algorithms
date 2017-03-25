package com.algorithms.tree.path

class Tree(var x: Int, var l: Tree, var r: Tree)

sealed trait MyTree
case class Node(value: Int, left: MyTree, right: MyTree) extends MyTree
case object Leaf extends MyTree

object Solution {

  import scala.collection.mutable.Set

  def solution(t: Tree): Int =
    mostDistinctPath(toMyTree(t))

  private def mostDistinctPath(t: MyTree): Int = {
    val paths = distinctPaths(t)
    val max = paths.reduce { (s, acc) =>
      if (s.size > acc.size)
        s
      else
        acc
    }

    max.size
  }

  private def distinctPaths(t: MyTree): List[Set[Int]] = t match {
    case Leaf =>
      List(Set[Int]())
    case Node(v, l, r) =>
      val leftPaths = distinctPaths(l)
      val rightPaths = distinctPaths(r)

      leftPaths map { _ += v }
      rightPaths map { _ += v }

      leftPaths ::: rightPaths
  }

  private def toMyTree(t: Tree): MyTree = {
    if (t == null)
      Leaf
    else {
      val left: MyTree =
        if (t.l == null)
          Leaf
        else
          toMyTree(t.l)

      val right: MyTree =
        if (t.r == null)
          Leaf
        else
          toMyTree(t.r)

      Node(t.x, left, right)
    }
  }

}
