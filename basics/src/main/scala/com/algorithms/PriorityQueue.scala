package com.algorithms

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import PriorityQueue.buildHeap
import PriorityQueue.heapify
import PriorityQueue.parent

object PriorityQueueMain extends App {

  println("")

  val seq = IndexedSeq(8, 14, 16, 4, 1, 10, 3, 9, 7, 2)
  println("Sequence: " + seq)

  val queue = PriorityQueue(seq: _*)
  println("Queue: " + queue)

  println("")

  val maximum = queue.peek
  println("Maximum: " + maximum.getOrElse("<empty queue>"))

  println("")

  queue.poll match {
    case Some(_) =>
      println("After polling: " + queue)
    case None =>
      println("After polling: <empty heap>")
  }

  println("")

  println("Increasing key for index 8 with value 1 to value 11...")
  queue.update(8, 11) match {
    case Success(_) =>
      println("After increasing key: " + queue)
    case Failure(e) =>
      throw new Exception(e)
  }

  println("")

  println("Inserting key 23...")
  queue.offer(23)
  println("After inserting key: " + queue)

  println("")

  println("Removing key with index 2...")
  queue.remove(2)
  println("After removing key: " + queue)

  println("")

  val minQueue = PriorityQueue.MinPriorityQueue(seq: _*)
  println("Min queue: " + minQueue)

  println("")
}

sealed trait PriorityQueue[A] {
  implicit def ord: Ordering[A]
  implicit def ct: ClassTag[A]

  def elems: IndexedSeq[A]
  def f: (A, A) => Boolean

  private var heapSize: Int =
    elems.size

  private var ar: Array[A] = {
    val length = if (elems.size > 100)
      2 * elems.size
    else
      100

    val a: Array[A] = new Array(length)
    elems copyToArray a

    if (heapSize > 1)
      buildHeap(a, heapSize)(f)
    else
      a
  }

  def peek: Option[A] = heapSize match {
    case 0 =>
      None
    case _ =>
      Some(ar(0))
  }

  def poll: Option[A] = heapSize match {
    case 0 =>
      None
    case _ =>
      val max = ar(0)
      ar(0) = ar(heapSize - 1)
      heapSize = heapSize - 1
      ar = heapify(ar, 0, heapSize)(f)

      Some(max)
  }

  def update(i: Int, key: A): Try[Unit] = (i, key) match {
    case (i, _) if (i < 0 || i >= heapSize) =>
      Failure(new IndexOutOfBoundsException())
    case (i, key) if (ord lt (key, ar(i))) =>
      Failure(new Exception("New key is smaller then current key"))
    case _ =>
      ar(i) = key
      @tailrec
      def go(k: Int, p: Int): Array[A] = (k, p) match {
        case (0, _) =>
          ar
        case (_, p) if (ord gteq (ar(p), ar(k))) =>
          ar
        case _ =>
          val tmp = ar(p)
          ar(p) = ar(k)
          ar(k) = tmp

          go(p, parent(p))
      }
      ar = go(i, parent(i))

      Success(())
  }

  def offer(key: A): Unit = {
    heapSize = heapSize + 1
    if (heapSize > ar.size)
      insureCapacity

    ar(heapSize - 1) = key
    update(heapSize - 1, key)
  }

  def remove(i: Int): Try[Unit] = i match {
    case i if (i < 0 || i >= heapSize) =>
      Failure(new IndexOutOfBoundsException())
    case _ =>
      ar(i) = ar(heapSize - 1)
      heapSize = heapSize - 1
      ar = heapify(ar, i, heapSize)(f)

      Success(())
  }

  def heap: IndexedSeq[A] =
    ar.take(heapSize).toIndexedSeq

  override def toString: String =
    heap mkString ("[", ", ", "]")

  private def insureCapacity: Unit = {
    val a: Array[A] = new Array(2 * ar.size)
    ar copyToArray a

    ar = a
  }

}

case class MaxPriorityQueue[A](elems: IndexedSeq[A], ord: Ordering[A], ct: ClassTag[A]) extends PriorityQueue[A] {
  def f: (A, A) => Boolean =
    (x, y) => {
      if (ord.compare(x, y) > 0)
        true
      else
        false
    }
}

case class MinPriorityQueue[A](elems: IndexedSeq[A], ord: Ordering[A], ct: ClassTag[A]) extends PriorityQueue[A] {
  def f: (A, A) => Boolean =
    (x, y) => {
      if (ord.compare(x, y) < 0)
        true
      else
        false
    }
}

object PriorityQueue {

  implicit object IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int): Int =
      x - y
  }

  def apply[A](elems: A*)(implicit ord: Ordering[A], ct: ClassTag[A]): PriorityQueue[A] =
    MaxPriorityQueue(elems: _*)(ord, ct)

  def MaxPriorityQueue[A](elems: A*)(implicit ord: Ordering[A], ct: ClassTag[A]): MaxPriorityQueue[A] =
    new MaxPriorityQueue(IndexedSeq(elems: _*), ord, ct)

  def MinPriorityQueue[A](elems: A*)(implicit ord: Ordering[A], ct: ClassTag[A]): MinPriorityQueue[A] =
    new MinPriorityQueue(IndexedSeq(elems: _*), ord, ct)

  private def buildHeap[A](ar: Array[A], heapSize: Int)(f: (A, A) => Boolean): Array[A] = {
    var a = ar
    val half = heapSize / 2

    for {
      i <- (half - 1) to 0 by -1
    } a = heapify(a, i, heapSize)(f)

    a
  }

  @tailrec
  private def heapify[A](ar: Array[A], i: Int, heapSize: Int)(f: (A, A) => Boolean): Array[A] = {
    val l = left(i)
    val r = right(i)
    var largest = i

    if (l < heapSize && f(ar(l), ar(i)))
      largest = l
    if (r < heapSize && f(ar(r), ar(largest)))
      largest = r

    if (largest == i)
      ar
    else {
      val tmp = ar(i)
      ar(i) = ar(largest)
      ar(largest) = tmp

      heapify(ar, largest, heapSize)(f)
    }
  }

  private def parent(i: Int): Int =
    ((i + 1) / 2) - 1

  private def left(i: Int): Int =
    (2 * i) + 1

  private def right(i: Int): Int =
    (2 * i) + 2

}
