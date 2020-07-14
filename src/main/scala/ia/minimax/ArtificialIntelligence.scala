package ia.minimax

import akka.actor.{Actor, PoisonPill, Props}
import ia.messages.Messages._
import ia.minimax.RootActor.{MaxRootActor, MinRootActor}
import model.game.Level.Level
import model.game.Player.Player
import model._
import model.game.{GameSnapshot, Player}

object ArtificialIntelligence {
  def apply(model: ModelHnefatafl, levelIA: Level): ArtificialIntelligence = new ArtificialIntelligence(model, levelIA)
}

case class ArtificialIntelligence(model: ModelHnefatafl, levelIA: Level) extends Actor {

  override def receive: Receive = {
    case event: FindBestMoveMsg => findBestMove(event.gameSnapshot)
    case event: ReturnBestMoveMsg => model.iaBestMove(event.bestMove); context.stop(sender());
    case _: CloseMsg => context.become(dyingState)
  }

  /**
    * Terminal behaviour of the IA actor
    */
  def dyingState: Receive = {
    case _: ReturnBestMoveMsg => context.stop(sender()); self ! PoisonPill
  }

  /**
    * Creates Maximizing/Minimizing Actor according to IA player.
    */
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