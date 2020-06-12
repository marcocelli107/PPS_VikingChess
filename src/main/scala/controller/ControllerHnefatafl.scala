package controller

import java.util

import scala.collection.JavaConverters._
import util.List

import model.{GameVariant, ModelHnefatafl, Player}
import model.ModelHnefatafl.ModelHnefataflImpl
import utils.Board.Board
import utils.Pair

import view.{GameViewImpl}

trait ControllerHnefatafl {

  /**
    * Calls model for a new game.
    *
    * @return board
    */
  def newGame(variant: GameVariant.Val): Board

  /**
    * Calls model for the possible moves from a specified coordinate.
    *
    * @return list of coordinates
    */
  def getPossibleMoves(coordinate: Pair[Int]): List[Pair[Int]]

  /**
    * Calls model for sets a move selected from coordinate to coordinate.
    *
    * @return (board, numberBlackPiecesCaptured, numberWhitePiecesCaptured)
    */
  def setMove(coordinateStart: Pair[Int],coordinateArrival: Pair[Int]): (Board, Int, Int)

  /**
    * Calls view for indicate the winner of the game.
    */
  def gameEnded(winner: Player.Value): Unit
}

object ControllerHnefatafl {

  def apply: ControllerHnefatafl = ControllerHnefataflImpl()

  case class ControllerHnefataflImpl() extends ControllerHnefatafl {

    private val viewGame: GameViewImpl = new GameViewImpl(this)
    private val modelGame: ModelHnefatafl = ModelHnefataflImpl(this)

    override def newGame(variant: GameVariant.Val): Board = modelGame.createGame(variant)

    override def getPossibleMoves(coordinate: Pair[Int]): List[Pair[Int]] = modelGame.showPossibleCells(coordinate).asJava

    override def setMove(coordinateStart: Pair[Int],coordinateArrival: Pair[Int]): (Board, Int, Int) = modelGame.setMove(coordinateStart, coordinateArrival)

    override def gameEnded(winner: Player.Value): Unit = ???
  }
}

