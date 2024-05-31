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
}


case object TreeEnd extends BinaryTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def leftChild: BinaryTree[Nothing] = throw new NoSuchElementException

  override def rightChild: BinaryTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BinaryTree[Nothing]] = List()
}
