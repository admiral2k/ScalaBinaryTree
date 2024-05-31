package dev

import scala.::
import scala.annotation.tailrec

abstract class BinaryTree[+T] {
  def value: T // значение узла

  def leftChild: BinaryTree[T] // левый потомок

  def rightChild: BinaryTree[T] // правый потомок


  def isEmpty: Boolean

  def isLeaf: Boolean

  def collectLeaves: List[BinaryTree[T]]

  def countLeaves: Int

  def nodesAtLevel(level: Int): List[BinaryTree[T]]
}

case class Node[+T](
                     override val value: T,
                     override val leftChild: BinaryTree[T],
                     override val rightChild: BinaryTree[T])
  extends BinaryTree[T] {

  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = leftChild.isEmpty && rightChild.isEmpty

  override def collectLeaves: List[BinaryTree[T]] = {
    @tailrec
    def loop(toInspect: List[BinaryTree[T]] = List(this), leaves: List[BinaryTree[T]] = List()): List[BinaryTree[T]] = {
      if (toInspect.isEmpty) leaves
      else {
        if (toInspect.head.isLeaf) loop(toInspect.tail, leaves :+ toInspect.head)
        else {
          if (toInspect.head.leftChild.isEmpty && !toInspect.head.rightChild.isEmpty) {
            loop(toInspect.tail :+ toInspect.head.rightChild, leaves)
          }
          else if (!toInspect.head.leftChild.isEmpty && toInspect.head.rightChild.isEmpty) {
            loop(toInspect.tail :+ toInspect.head.leftChild, leaves)
          }
          else {
            loop(toInspect.tail :+ toInspect.head.leftChild :+ toInspect.head.rightChild, leaves)
          }
        }
      }
    }

    loop()
  }

  override def countLeaves: Int = collectLeaves.size

  override def nodesAtLevel(level: Int): List[BinaryTree[T]] = {
    @tailrec
    def loop(nodesAtCurrentLevel: List[BinaryTree[T]] = List(this), levelsToGo: Int = level): List[BinaryTree[T]] = {
      if (levelsToGo < 0) Nil
      else if (levelsToGo == 0) nodesAtCurrentLevel
      else loop(nodesAtCurrentLevel.flatMap(node => List(node.leftChild, node.rightChild)).filterNot(_.isEmpty), levelsToGo - 1)
    }

    loop()
  }
}


case object TreeEnd extends BinaryTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def leftChild: BinaryTree[Nothing] = throw new NoSuchElementException

  override def rightChild: BinaryTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BinaryTree[Nothing]] = List()

  override def countLeaves: Int = 0

  override def nodesAtLevel(level: Int): List[BinaryTree[Nothing]] = Nil
}
