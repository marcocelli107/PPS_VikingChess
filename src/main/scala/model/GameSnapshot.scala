package model

import model.GameVariant.GameVariant
import model.Player.Player
import utils.BoardGame.Board
import utils.Move

trait GameSnapshot {

  def getVariant: GameVariant

  def getPlayerToMove: Player

  def getWinner: Player

  def getBoard: Board

  /**
    * Returns the coordinates of the last move.
    *
    * @return
    *         from coordinate - to coordinate
    */
  def getLastMove: Option[Move]

  def getNumberCapturedBlacks: Int

  def getNumberCapturedWhites: Int

  def getCopy: GameSnapshot
}

object GameSnapshot {

  def apply(variant: GameVariant, playerToMove: Player, winner: Player, board: Board, lastMove: Option[Move], numberCapturedBlacks: Int, numberCapturedWhites: Int): GameSnapshot =
    GameSnapshotImpl(variant, playerToMove, winner, board, lastMove, numberCapturedBlacks, numberCapturedWhites)

  case class GameSnapshotImpl(variant: GameVariant, playerToMove: Player, winner: Player, board: Board, lastMove: Option[Move], numberCapturedBlacks: Int, numberCapturedWhites: Int) extends GameSnapshot {

    override def getVariant: GameVariant = variant

    override def getPlayerToMove: Player = playerToMove

    override def getWinner: Player = winner

    override def getBoard: Board = board

    override def getLastMove: Option[Move] = lastMove

    override def getNumberCapturedBlacks: Int = numberCapturedBlacks

    override def getNumberCapturedWhites: Int = numberCapturedWhites

    override def getCopy: GameSnapshot = GameSnapshotImpl(variant, playerToMove, winner, board.getCopy, lastMove, numberCapturedBlacks, numberCapturedWhites)
  }
}
