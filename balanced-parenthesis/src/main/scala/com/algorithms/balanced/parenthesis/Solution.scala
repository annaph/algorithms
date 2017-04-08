package com.algorithms.balanced.parenthesis

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Solution {

  import BalancedParenthesis.isBalanced

  def solution(s: String): Boolean =
    isBalanced(s) match {
      case Success(v) =>
        v
      case Failure(e) =>
        throw new Exception(e)
    }

}

object BalancedParenthesis {

  def isBalanced(s: String): Try[Boolean] =
    Try { _process(s) } match {
      case Success(v) =>
        Success(v)
      case Failure(e) =>
        Failure(e)
    }

  private def _process(s: String): Boolean = {
    @tailrec
    def loop(cs: List[Char], buf: List[Char]): Boolean = cs match {
      case Nil =>
        if (buf.isEmpty)
          true
        else
          false
      case h :: t =>
        h match {
          case c if (c == '{' || c == '[' || c == '(') =>
            loop(t, c :: buf)
          case c if (c == '}' || c == ']' || c == ')') =>
            buf.headOption match {
              case Some(v) if (v == left(c)) =>
                loop(t, buf.tail)
              case _ =>
                false
            }
          case _ =>
            loop(t, buf)
        }
    }

    loop(s.toList, List())
  }

  private def left(c: Char): Char = c match {
    case '}' =>
      '{'
    case ']' =>
      '['
    case _ =>
      '('
  }

}
