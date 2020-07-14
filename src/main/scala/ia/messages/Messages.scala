package ia.messages

import model.game.{GameSnapshot, Move}

object Messages {

  /**
   * Message used by the IA to return at model the best computed move
   *
   * @param bestMove
   *                 best move
   */
  case class ReturnBestMoveMsg(bestMove: Move)

  /**
   * Message used by the model to communicate at IA to start of the search for the best move for that snapshot
   *
   * @param gameSnapshot
   *                     game snapshot
   */
  case class FindBestMoveMsg(gameSnapshot: GameSnapshot)

  /**
   * Message used to change behaviour of the actor to dying.
   */
  case class CloseMsg()

  /**
   * Message notifying the end of the delay time
   */

  case class EndTimeMsg()

  /**
   * Message notifying timer actor to activate
   */
  case class StartMsg()

}
