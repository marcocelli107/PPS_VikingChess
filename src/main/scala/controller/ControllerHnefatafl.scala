package controller

import java.util
import scala.collection.JavaConverters._
import model.{GameVariant, ModelHnefatafl, Player}
import view.GameViewImpl
import utils.BoardGame.Board
import utils.Pair


import scala.collection.mutable.ListBuffer

trait ControllerHnefatafl {

  /**
    * Calls model for a new game.
    *
    * @return board and player to move.
    */
  def newGame(variant: GameVariant.Val): (Board, Player.Value)

  /**
    * Calls model for the possible moves from a specified coordinate.
    *
    * @return list of coordinates
    */
  def getPossibleMoves(coordinate: Pair[Int]): util.List[Pair[Int]]

  /**
    * Calls model for sets a move selected from coordinate to coordinate.
    *
    * @return (board, numberBlackPiecesCaptured, numberWhitePiecesCaptured)
    */
  def setMove(coordinateStart: Pair[Int],coordinateArrival: Pair[Int]): Unit

  /**
    * Calls view for indicate the winner of the game.
    */
  def gameEnded(winner: Player.Value, kingCoordinate: ListBuffer[Pair[Int]]): Unit

  /**
    * Notifies the view that the move has been updated.
    */
  def notifyMove(board: Board, numberBlackCaptured: Int, numberWhiteCaptured: Int): Unit
}

object ControllerHnefatafl {

  def apply: ControllerHnefatafl = ControllerHnefataflImpl()

  case class ControllerHnefataflImpl() extends ControllerHnefatafl {

    private val viewGame: GameViewImpl = new GameViewImpl(this)
    private var modelGame: ModelHnefatafl = _

    override def newGame(variant: GameVariant.Val): (Board, Player.Value) = {
      modelGame = ModelHnefatafl(this)
      modelGame.createGame(variant)
    }

    override def getPossibleMoves(coordinate: Pair[Int]): util.List[Pair[Int]] = modelGame.showPossibleCells(coordinate).asJava

    override def setMove(coordinateStart: Pair[Int],coordinateArrival: Pair[Int]): Unit = {
      modelGame.setMove(coordinateStart, coordinateArrival)
    }

    override def notifyMove(board: Board, numberBlackCaptured: Int, numberWhiteCaptured: Int): Unit = {
      viewGame.updateMove(board, numberBlackCaptured, numberWhiteCaptured)
    }

    override def gameEnded(winner: Player.Value, kingCoordinate: ListBuffer[Pair[Int]]): Unit = {
      if(winner.equals(Player.Draw))
        viewGame.setEndGame(winner.toString, null)
      else
        viewGame.setEndGame(winner.toString, kingCoordinate.asJava)
    }
  }
}

