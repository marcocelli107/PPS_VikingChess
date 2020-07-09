package ia.minimax

import akka.actor.{Actor, PoisonPill, Props}
import ia.minimax.RootActor.{MaxRootActor, MinRootActor}
import model.game.Level.Level
import model.game.Player.Player
import model._
import model.game.{GameSnapshot, Move, Player}

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

case class CloseMsg()

object ArtificialIntelligenceImpl {

  def apply(model: ModelHnefatafl, levelIA: Level): ArtificialIntelligenceImpl = new ArtificialIntelligenceImpl(model, levelIA)
}

case class ArtificialIntelligenceImpl(model: ModelHnefatafl, levelIA: Level) extends Actor {

  override def receive: Receive = {
    case event: FindBestMoveMsg => findBestMove(event.gameSnapshot)
    case event: ReturnBestMoveMsg => model.iaBestMove(event.bestMove); context.stop(sender()); self ! PoisonPill
    case _: CloseMsg => context.become(dyingState)
  }

  def dyingState: Receive = {
    case _: ReturnBestMoveMsg => context.stop(sender()); self ! PoisonPill
  }

  def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    var sonActor: Props = Props.empty
    if (iaIsBlack(gameSnapshot.getPlayerToMove))
      sonActor = Props(MinRootActor(levelIA))
    else
      sonActor = Props(MaxRootActor(levelIA))
    val refSonActor = context.actorOf(sonActor)
    refSonActor ! InitMsg(gameSnapshot.getCopy, levelIA.depth, Option.empty)

  }

  private def iaIsBlack(iaPlayer: Player): Boolean = iaPlayer.equals(Player.Black)

}