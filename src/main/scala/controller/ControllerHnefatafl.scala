package controller

import model.{GameSnapshot, GameVariant, ModelHnefatafl, Player, Snapshot}
import view.ViewHnefatafl
import utils.BoardGame.Board
import utils.Coordinate

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
  def getPossibleMoves(coordinate: Coordinate): Seq[Coordinate]

  /**
    * Calls model for making a move from coordinate to coordinate.
    *
    * @return (board, numberBlackPiecesCaptured, numberWhitePiecesCaptured)
    */
  def makeMove(coordinateStart: Coordinate, coordinateArrival: Coordinate): Unit

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
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
    * Checks if the cell at the specified coordinate is a init pawn cell.
    *
    * @param coordinate
    *                   coordinate of the cell to inspect
    *
    * @return boolean.
    */
  def isPawnCell(coordinate: Coordinate): Boolean

  /**
    * Find king coordinate in the current board.
    *
    * @return king coordinate to list.
    */
  def findKing(): Coordinate

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

    override def getPossibleMoves(coordinate: Coordinate): Seq[Coordinate] = modelGame.showPossibleCells(coordinate)

    override def makeMove(coordinateStart: Coordinate, coordinateArrival: Coordinate): Unit = {
      modelGame.makeMove(coordinateStart, coordinateArrival)
    }

    override def updateView(gameSnapshot: GameSnapshot): Unit = viewGame.update(gameSnapshot)

    override def isCentralCell(coordinate: Coordinate): Boolean = modelGame.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Coordinate): Boolean = modelGame.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Coordinate): Boolean = modelGame.isPawnCell(coordinate)

    override def findKing(): Coordinate = modelGame.findKing()

    override def changeSnapshot(snapshotToShow: Snapshot.Value): Unit = modelGame.changeSnapshot(snapshotToShow)

    override def undoMove(): Unit = modelGame.undoMove()
  }
}

