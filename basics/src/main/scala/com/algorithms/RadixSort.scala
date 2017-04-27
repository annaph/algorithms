package com.algorithms

import scala.util.Try
import CountingSort.sortAscending
import CountingSort.sortDescending

sealed trait Radix[A] {
  type D

  type Entry = (A, IndexedSeq[D])
  type Entries = IndexedSeq[Entry]
  type Encode = Entry => Int

  def zero: D
  def entries(s: IndexedSeq[A]): Entries
  def digitToInt: D => Int

  def entryToInt(d: Int): Encode = ???

  def maxDigit(s: Entries): Int = ???

  def asc: Entries => Encode => Try[Entries] = {
    sortAscending[Entry] _
  }

  def desc: Entries => Encode => Try[Entries] = {
    sortDescending[Entry] _
  }

}

object RadixSort {

}
