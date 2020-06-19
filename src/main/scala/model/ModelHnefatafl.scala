package model

import scala.collection.mutable.ListBuffer
import controller.ControllerHnefatafl
import model.GameSnapshot.GameSnapshotImpl
import utils.BoardGame.Board
import utils.Coordinate

import scala.collection.mutable

trait ModelHnefatafl {

  /**
   * Defines the game variant.
   */
  var currentVariant: GameVariant.Val

  /**
   * Defines the chosen mode.
   */
  var mode: GameMode.Value

  /**
   * Calls parser for a new Game.
   *
   * @param variant
   * indicates the variant chosen by the user.
   * @return created board and player to move.
   */
  def createGame(variant: GameVariant.Val): (Board, Player.Val)

  /**
   * Calls parser for the possible moves from a cell.
   *
   * @param cell
   * coordinate of the Cell.
   * @return list buffer of the possible computed moves.
   */
  def showPossibleCells(cell: Coordinate): ListBuffer[Coordinate]

  /**
   * Calls parser for making a move from coordinate to coordinate.
   *
   * @param fromCoordinate
   * coordinate of the starting cell.
   * @param toCoordinate
   * coordinate of the arrival cell.
   * @return updated board.
   */
  def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): Unit

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   * coordinate of the cell to inspect
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   * coordinate of the cell to inspect
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a init pawn cell.
   *
   * @param coordinate
   * coordinate of the cell to inspect
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
   * indicates snapshot to show.
   * @return required board
   */
  def changeSnapshot(snapshotToShow: Snapshot.Value): Unit

  /**
   * Undoes last move.
   */
  def undoMove(): Unit
}

object ModelHnefatafl {

  def apply(controller: ControllerHnefatafl): ModelHnefatafl = ModelHnefataflImpl(controller)

  case class ModelHnefataflImpl(controller: ControllerHnefatafl) extends ModelHnefatafl {

    /**
     * Inits the parser prolog and set the file of the prolog rules.
     */
    private val THEORY: String = TheoryGame.GameRules.toString
    private val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
    private var storySnapshot: mutable.ListBuffer[GameSnapshot] = _
    private var currentSnapshot: Int = 0

    /**
     * Defines status of the current game.
     */
    private var game: (Player.Val, Player.Val, Board, Int) = _

    private final val SIZE_DRAW: Int = 9

    override var currentVariant: GameVariant.Val = _

    override var mode: GameMode.Value = GameMode.PVP

    override def createGame(newVariant: GameVariant.Val): (Board, Player.Val) = {

      currentVariant = newVariant

      game = parserProlog.createGame(currentVariant.nameVariant.toLowerCase)

      storySnapshot = mutable.ListBuffer(GameSnapshotImpl(currentVariant, game._1, game._2, game._3, Option.empty, 0, 0))

      (game._3, game._1)
    }

    override def showPossibleCells(cell: Coordinate): ListBuffer[Coordinate] = {
      if (showingCurrentSnapshot)
        parserProlog.showPossibleCells(cell)
      else ListBuffer.empty
    }

    override def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): Unit = {

      game = parserProlog.makeLegitMove(fromCoordinate, toCoordinate)

      val pieceCaptured: (Int, Int) = incrementCapturedPieces(game._1, game._4)
      var winner: Player.Val = game._2

      if (checkThreefoldRepetition())
        winner = Player.Draw

      storySnapshot += GameSnapshot(currentVariant, game._1, winner, game._3, Option(fromCoordinate, toCoordinate), pieceCaptured._1, pieceCaptured._2)

      currentSnapshot += 1

      controller.updateView(storySnapshot.last)
    }

    override def isCentralCell(coordinate: Coordinate): Boolean = parserProlog.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Coordinate): Boolean = parserProlog.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Coordinate): Boolean = parserProlog.isPawnCell(coordinate)

    override def findKing(): Coordinate = parserProlog.findKing()

    override def changeSnapshot(previousOrNext: Snapshot.Value): Unit = {
      previousOrNext match {
        case Snapshot.Previous => decrementCurrentSnapshot()
        case Snapshot.Next => incrementCurrentSnapshot()
        case Snapshot.First => currentSnapshot = 0
        case Snapshot.Last => currentSnapshot = storySnapshot.size - 1
      }
      val gameSnapshot = storySnapshot(currentSnapshot)
      controller.updateView(gameSnapshot)
    }

    override def undoMove(): Unit = {
      if (showingCurrentSnapshot) {
        val lastMove: Option[(Coordinate, Coordinate)] = storySnapshot.last.getLastMove
        if (lastMove.nonEmpty) {
          storySnapshot -= storySnapshot.last
          currentSnapshot -= 1
          parserProlog.undoMove(storySnapshot.last.getBoard)
          controller.updateView(storySnapshot.last)
        }
      }
    }

    /**
     * Increments the number of pieces captured of the player.
     */
    private def incrementCapturedPieces(player: Player.Val, piecesCaptured: Int): (Int, Int) = player match {
      case Player.Black => (storySnapshot.last.getNumberCapturedBlacks + piecesCaptured, storySnapshot.last.getNumberCapturedWhites)
      case Player.White => (storySnapshot.last.getNumberCapturedBlacks, storySnapshot.last.getNumberCapturedWhites + piecesCaptured)
    }

    /**
     * Checks if there was a threefold repetition.
     *
     * @return boolean
     */
    private def checkThreefoldRepetition(): Boolean = storySnapshot.reverse.take(SIZE_DRAW) match {
      case l if l.isEmpty || l.size < SIZE_DRAW => false
      case l if l.head.equals(l(4)) && l(4).equals(l(8)) => true
      case _ => false
    }

    private def incrementCurrentSnapshot(): Unit = if (!showingCurrentSnapshot) currentSnapshot += 1

    private def decrementCurrentSnapshot(): Unit = if (currentSnapshot > 0) currentSnapshot -= 1

    private def showingCurrentSnapshot: Boolean = currentSnapshot == storySnapshot.size - 1
  }

}
