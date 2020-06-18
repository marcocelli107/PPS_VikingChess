package ia

import utils.BoardGame.Board

import scala.collection.mutable

case class Node(board: Board, score: Option[Int])

trait GameTree{

  def addNode(node: MiniMaxNode) : Unit

  def addChild(keyNode: MiniMaxNode, childNode: MiniMaxNode) : Unit

  def removeSingleNode(node: MiniMaxNode) : Unit

  def updateFather(childNode: MiniMaxNode, fatherNode: MiniMaxNode): Unit

  def getMapFatherChildren: mutable.Map[MiniMaxNode, mutable.Seq[MiniMaxNode]]

  def getMapChildFather: mutable.Map[MiniMaxNode, MiniMaxNode]

  def deleteUnsudedChilds(fatherNode:MiniMaxNode, chosenNode:MiniMaxNode):Unit

  def getChildren( fatherNode: MiniMaxNode): mutable.Seq[MiniMaxNode]
}

object GameTree {

  def apply(): GameTreeImpl = new GameTreeImpl

  case class GameTreeImpl() extends GameTree {

    var mapChildFather: mutable.Map[MiniMaxNode, MiniMaxNode] = mutable.HashMap()

    var mapFatherChildren: mutable.Map[MiniMaxNode, mutable.Seq[MiniMaxNode]] = mutable.HashMap()

    def addNode(node: MiniMaxNode): Unit = {
      val list: mutable.Seq[MiniMaxNode] = mutable.Seq()
      mapFatherChildren += node -> list
    }

    def removeSingleNode(node: MiniMaxNode):Unit = {
      val nodeToRemove = node
      mapFatherChildren -= nodeToRemove
    }

    def addChild(keyNode: MiniMaxNode, childNode: MiniMaxNode): Unit = {
      mapFatherChildren(keyNode) = mapFatherChildren(keyNode) :+ childNode
      updateFather(childNode, keyNode)
    }

    def updateFather(childNode: MiniMaxNode, fatherNode: MiniMaxNode): Unit = {
      mapChildFather += childNode -> fatherNode
    }

    def deleteUnsudedChilds(fatherNode:MiniMaxNode, chosenNode:MiniMaxNode):Unit = {
      mapChildFather-= fatherNode
      mapFatherChildren(fatherNode).filter(node => !node.equals(chosenNode)).foreach( node => deleteUnsudedChilds(node, chosenNode ))
      removeSingleNode(fatherNode)
    }

    def getMapFatherChildren: mutable.Map[MiniMaxNode, mutable.Seq[MiniMaxNode]] = mapFatherChildren

    def getMapChildFather: mutable.Map[MiniMaxNode, MiniMaxNode] = mapChildFather

    def getChildren( fatherNode: MiniMaxNode): mutable.Seq[MiniMaxNode] = mapFatherChildren(fatherNode)

  }
}