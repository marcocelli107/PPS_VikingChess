package controller

import model.{GameSnapshot, GameVariant, ModelHnefatafl, Player, Snapshot}
import view.ViewHnefatafl
import utils.BoardGame.Board
import utils.Pair

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
  def getPossibleMoves(coordinate: Pair[Int]): Seq[Pair[Int]]

  /**
    * Calls model for making a move from coordinate to coordinate.
    *
    * @return (board, numberBlackPiecesCaptured, numberWhitePiecesCaptured)
    */
  def makeMove(coordinateStart: Pair[Int], coordinateArrival: Pair[Int]): Unit

  /**
    * Notifies the view that the move has been updated.
    *
    * @param gameSnapshot
    *                 snapshot to show.
    */
  def updateView(gameSnapshot: GameSnapshot): Unit

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCentralCell(coordinate: Pair[Int]): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Pair[Int]): Boolean

  /**
    * Checks if the cell at the specified coordinate is a init pawn cell.
    *
    * @param coordinate
    *                   coordinate of the cell to inspect
    *
    * @return boolean.
    */
  def isPawnCell(coordinate: Pair[Int]): Boolean

  /**
    * Find king coordinate in the current board.
    *
    * @return king coordinate to list.
    */
  def findKing(): Pair[Int]

  /**
    * Returns a previous or later state of the current board.
    *
    * @param snapshotToShow
    *                       indicates snapshot to show.
    */
  def changeSnapshot(snapshotToShow: Snapshot.Value): Unit

  /**
   * Undoes last move.
   */
  def undoMove(): Unit
}

object ControllerHnefatafl {

  def apply: ControllerHnefatafl = ControllerHnefataflImpl()

  case class ControllerHnefataflImpl() extends ControllerHnefatafl {

    private val viewGame: ViewHnefatafl = ViewHnefatafl(this)
    private var modelGame: ModelHnefatafl = _

    override def newGame(variant: GameVariant.Val): (Board, Player.Value) = {
      modelGame = ModelHnefatafl(this)
      modelGame.createGame(variant)
    }

    override def getPossibleMoves(coordinate: Pair[Int]): Seq[Pair[Int]] = modelGame.showPossibleCells(coordinate)

    override def makeMove(coordinateStart: Pair[Int], coordinateArrival: Pair[Int]): Unit = {
      modelGame.makeMove(coordinateStart, coordinateArrival)
    }

    override def updateView(gameSnapshot: GameSnapshot): Unit = viewGame.update(gameSnapshot)

    override def isCentralCell(coordinate: Pair[Int]): Boolean = modelGame.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Pair[Int]): Boolean = modelGame.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Pair[Int]): Boolean = modelGame.isPawnCell(coordinate)

    override def findKing(): Pair[Int] = modelGame.findKing()

    override def changeSnapshot(snapshotToShow: Snapshot.Value): Unit = modelGame.changeSnapshot(snapshotToShow)

    override def undoMove(): Unit = modelGame.undoMove()
  }
}

