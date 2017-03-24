package com.algorithms.digetless.password

object Solution {

  def solution(s: String): Int = {
    digitlessPassword(s) match {
      case Some(v) =>
        v.length
      case None =>
        -1
    }
  }

  private def digitlessPassword(str: String): Option[String] = {
    val password = """[a-z]*[A-Z][a-z]*""".r

    (password findAllIn str).toList match {
      case l: List[String] if l.nonEmpty =>
        val max = l.reduce { (s, acc) =>
          if (s.length > acc.length)
            s
          else
            acc
        }
        Some(max)
      case Nil =>
        None
    }
  }

}
