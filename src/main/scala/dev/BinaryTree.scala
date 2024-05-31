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

  def collectNodes: List[BinaryTree[T]]

  def hasPath(tree: BinaryTree[Int], target: Int): Boolean

  def findAllPaths(tree: BinaryTree[String], target: String): List[List[String]]
}

case class Node[+T](
                     override val value: T,
                     override val leftChild: BinaryTree[T],
                     override val rightChild: BinaryTree[T])
  extends BinaryTree[T] {

  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = leftChild.isEmpty && rightChild.isEmpty

  override def collectLeaves: List[BinaryTree[T]] = {
    if (this.isLeaf) List(this)
    else this.leftChild.collectLeaves ::: this.rightChild.collectLeaves
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

  override def collectNodes: List[BinaryTree[T]] = {
    this :: this.leftChild.collectNodes ::: this.rightChild.collectNodes
  }


    //TODO: edit to the following signature
    //TODO: def hasPath(target: Int): Boolean
  def hasPath(tree: BinaryTree[Int], target: Int): Boolean = {
    if (tree.isEmpty) false
    else if (tree.isLeaf) tree.value == target
    else hasPath(tree.leftChild, target - tree.value) || hasPath(tree.rightChild, target - tree.value)
  }

  //TODO: edit to the following signature
  //TODO: def findAllPaths(target: Int): List[List[BinaryTree[T]]]
  def findAllPaths(tree: BinaryTree[String], target: String): List[List[String]] = {
    def loop(currentNode: BinaryTree[String],
             currentPath: List[String],
             targetLeft: Int): List[List[String]] = {
      if (currentNode.isEmpty) {
        List()
      } else {
        val newTargetLeft = targetLeft - currentNode.value.toInt
        val newPath = currentPath :+ currentNode.value

        if (currentNode.isLeaf && newTargetLeft == 0) {
          List(newPath)
        } else {
          val leftPaths = loop(currentNode.leftChild, newPath, newTargetLeft)
          val rightPaths = loop(currentNode.rightChild, newPath, newTargetLeft)
          leftPaths ++ rightPaths
        }
      }
    }

    loop(tree, List(), target.toInt)
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

  override def collectNodes(): List[BinaryTree[Nothing]] = List()


  def hasPath(tree: BinaryTree[Int], target: Int): Boolean = false

  def findAllPaths(tree: BinaryTree[String], target: String): List[List[String]] = List()
}
