package model.game

import model.game.BoardGame.Board
import model.game.GameVariant.GameVariant
import model.game.Player.Player

trait GameSnapshot {

  /**
    * Gets game's variant.
    *
    * @return variant
    */
  def getVariant: GameVariant

  /**
    * Gets current game's player to move.
    *
    * @return player to move
    */
  def getPlayerToMove: Player

  /**
    * Gets current game's winner.
    *
    * @return winner
    */
  def getWinner: Player

  /**
    * Gets current game's board.
    *
    * @return variant
    */
  def getBoard: Board

  /**
    * Returns the coordinates of the last move.
    *
    * @return option from coordinate - to coordinate
    */
  def getLastMove: Option[Move]

  /**
    * Gets current number of captured blacks.
    *
    * @return int
    */
  def getNumberCapturedBlacks: Int

  /**
    * Gets current number of captured whites.
    *
    * @return int
    */
  def getNumberCapturedWhites: Int

  /**
    * Gets a game snapshot's copy.
    *
    * @return game snapshot
    */
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
