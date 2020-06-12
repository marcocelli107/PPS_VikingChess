package model

import controller.ControllerHnefatafl
import model.ParserProlog.ParserPrologImpl
import utils.Board.Board
import utils.Pair

import scala.collection.mutable.ListBuffer

trait ModelHnefatafl {

  /**
    * Defines the game variant.
    */
  var currentVariant: GameVariant.Val

  /**
    * Defines the chosen mode.
    */
  var mode: ModeGame.Value

  /**
    * Calls parser for a new Game.
    *
    * @return created board
    */
  def createGame(variant: GameVariant.Val): Board

  /**
    * Calls parser for the possible moves from a cell.
    * @param cell
    *                 coordinate of the Cell.
    *
    * @return list buffer of the possible computed moves.
    */
  def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]]

  /**
    * Calls parser for sets player move
    * @param cellStart
    *                 coordinate of the starting cell.
    * @param cellArrival
    *                 coordinate of the arrival cell.
    *
    * @return updated board.
    */
  def setMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Board,Int, Int)
}

object ModelHnefatafl {

  def apply(controller: ControllerHnefatafl): ModelHnefatafl = ModelHnefataflImpl(controller)

  case class ModelHnefataflImpl(controller: ControllerHnefatafl) extends ModelHnefatafl {

    /**
      * Inits the parser prolog and set the file of the prolog rules.
      */
    private val THEORY: String = TheoryGame.GameRules.toString
    private val parserProlog: ParserProlog = ParserPrologImpl(THEORY)

    /**
      * Defines status of the current game.
      */
    private var game: (Player.Value,Player.Value,Board, Int) = _

    /**
      * Number of white pieces captured.
      */
    private var numberWhiteCaptured: Int = 0

    /**
      * Number of black pieces captured.
      */
    private var numberBlackCaptured: Int = 0

    override var currentVariant: GameVariant.Val = _

    override var mode: ModeGame.Value = ModeGame.PVP

    override def createGame(newVariant: GameVariant.Val): Board = {

      currentVariant = newVariant

      game = parserProlog.createGame(currentVariant.nameVariant.toLowerCase)

      game._3
    }

    override def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]] = parserProlog.showPossibleCells(cell)

    override def setMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Board,Int, Int) = {

      game = parserProlog.makeMove(cellStart, cellArrival)

      incrementCapturedPieces(game._1, game._4)

      if(!game._2.equals(Player.None))
        controller.gameEnded(game._2)

      (game._3, numberBlackCaptured, numberWhiteCaptured)
    }

    /**
      * Increments the number of pieces captured of the player.
      */
    private def incrementCapturedPieces(player: Player.Value, piecesCaptured: Int): Unit = player match {
      case Player.Black => numberBlackCaptured += piecesCaptured
      case Player.White => numberWhiteCaptured += piecesCaptured
    }

    /**
      * Checks if precedent player has won.
      *
      * @return boolean
      */
    private def hasWon(possibleWinner: Player.Value): Boolean = !possibleWinner.equals(Player.None)
  }
}
