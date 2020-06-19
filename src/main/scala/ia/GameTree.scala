package ia

import model.ParserProlog

import scala.collection.mutable

case class Node(gameState: ParserProlog, score: Option[Int])

trait GameTree{

  def addNode(node: Node) : Unit

  def addChild(keyNode: Node, childNode: Node) : Unit

  def removeSingleNode(node: Node) : Unit

  def updateFather(childNode: Node, fatherNode: Node): Unit

  def getMapFatherChildren: mutable.Map[Node, mutable.Seq[Node]]

  def getMapChildFather: mutable.Map[Node, Node]

  def deleteUnsudedChilds(fatherNode:Node, chosenNode:Node):Unit

  def getChildren( fatherNode: Node): mutable.Seq[Node]
}

object GameTree {

  def apply(): GameTreeImpl = new GameTreeImpl

  case class GameTreeImpl() extends GameTree {

    var mapChildFather: mutable.Map[Node, Node] = mutable.HashMap()

    var mapFatherChildren: mutable.Map[Node, mutable.Seq[Node]] = mutable.HashMap()

    def addNode(node: Node): Unit = {
      val list: mutable.Seq[Node] = mutable.Seq()
      mapFatherChildren += node -> list
    }

    def removeSingleNode(node: Node):Unit = {
      val nodeToRemove = node
      mapFatherChildren -= nodeToRemove
    }

    def addChild(keyNode: Node, childNode: Node): Unit = {
      mapFatherChildren(keyNode) = mapFatherChildren(keyNode) :+ childNode
      updateFather(childNode, keyNode)
    }

    def updateFather(childNode: Node, fatherNode: Node): Unit = {
      mapChildFather += childNode -> fatherNode
    }

    def deleteUnsudedChilds(fatherNode:Node, chosenNode:Node):Unit = {
      mapChildFather-= fatherNode
      mapFatherChildren(fatherNode).filter(node => !node.equals(chosenNode)).foreach( node => deleteUnsudedChilds(node, chosenNode ))
      removeSingleNode(fatherNode)
    }

    def getMapFatherChildren: mutable.Map[Node, mutable.Seq[Node]] = mapFatherChildren

    def getMapChildFather: mutable.Map[Node, Node] = mapChildFather

    def getChildren( fatherNode: Node): mutable.Seq[Node] = mapFatherChildren(fatherNode)

  }
}