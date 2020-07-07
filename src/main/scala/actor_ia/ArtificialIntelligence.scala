package actor_ia

import actor_ia.RootActor.{MaxRootActor, MinRootActor}
import akka.actor.{Actor, Props}
import model._
import utils.Move

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

object ArtificialIntelligenceImpl {

  def apply(model: ModelHnefatafl, depth: Int, levelIA: Level.Val): ArtificialIntelligenceImpl = new ArtificialIntelligenceImpl(model, depth, levelIA)
}

case class ArtificialIntelligenceImpl(model: ModelHnefatafl, depth: Int, levelIA: Level.Val) extends Actor {

  override def receive: Receive = {
    case event: FindBestMoveMsg => findBestMove(event.gameSnapshot)
    case event: ReturnBestMoveMsg => model.makeMove(event.bestMove); context.stop(sender())
  }

  def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    var sonActor: Props = Props.empty
    if (iaIsBlack(gameSnapshot.getPlayerToMove))
      sonActor = Props(MinRootActor(levelIA))
    else
      sonActor = Props(MaxRootActor(levelIA))
    val refSonActor = context.actorOf(sonActor)
    refSonActor ! InitMsg(gameSnapshot.getCopy, depth, Option.empty)

  }

  private def iaIsBlack(iaPlayer: Player.Val): Boolean = iaPlayer.equals(Player.Black)

}