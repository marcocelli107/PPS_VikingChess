package model.ia.minimax

import akka.actor.{Actor, PoisonPill, Props}
import model.ia.messages.Messages._
import model.ia.minimax.RootActor.{MaxRootActor, MinRootActor}
import model.game.Level.Level
import model.game.Player.Player
import model._
import model.game.{GameSnapshot, Move, Player}

object ArtificialIntelligence {
  def apply(model: ModelHnefatafl, levelIA: Level): ArtificialIntelligence = new ArtificialIntelligence(model, levelIA)
}

/**
 * Implementation of a minimax based viking chess artificial intelligence made with actors
 */
case class ArtificialIntelligence(model: ModelHnefatafl, levelIA: Level) extends Actor {

  /**
   * @inheritdoc
   */
  override def receive: Receive = {
    case event: FindBestMoveMsg => findBestMove(event.gameSnapshot);
    case event: ReturnBestMoveMsg => model.iaBestMove(event.bestMove); stopSender()
    case _: CloseMsg => context.become(dyingBehaviour)
  }

  /**
    * Terminal behaviour of the IA actor
    */
  def dyingBehaviour: Receive = {
    case _: ReturnBestMoveMsg => context.stop(sender()); suicide()
    case _: EndTimeMsg => context.stop(sender()); suicide()
  }

  /**
   * Behaviour in which the actor is waiting for the best move evaluated
   */
  def waitingBehaviour: Receive = {
    case _: EndTimeMsg =>  context.become(receive); stopSender();
    case event: ReturnBestMoveMsg => context.become(delayReturnBestMoveBehaviour(event.bestMove))
    case _: CloseMsg => context.become(preparationToDyingBehaviour)
  }

  /**
   * Behaviour in which the actor has to wait for the timer before returning the best move
   *
   * @param move
   *             best move evaluated
   */
  def delayReturnBestMoveBehaviour(move: Move): Receive = {
    case _: EndTimeMsg => context.become(receive) ; self ! ReturnBestMoveMsg(move)
    case _: CloseMsg => context.become(dyingBehaviour)
  }

  /**
   * Behaviour in which the actor is waiting for the timer or the best move before switching to dying behaviour
   */
  def preparationToDyingBehaviour: Receive = {
    case _: ReturnBestMoveMsg => stopSender(); context.become(dyingBehaviour)
    case _: EndTimeMsg => stopSender(); context.become(dyingBehaviour)
  }

  private def stopSender(): Unit = if (!context.sender().equals(self)) context.stop(sender())

  private def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    var sonActor: Props = Props.empty
    if (iaIsBlack(gameSnapshot.getPlayerToMove))
      sonActor = Props(MinRootActor(levelIA))
    else
      sonActor = Props(MaxRootActor(levelIA))
    val refSonActor = context.actorOf(sonActor)
    refSonActor ! InitMsg(gameSnapshot.getCopy, levelIA.depth, Option.empty)

    context.become(waitingBehaviour)
    context.actorOf(Props(TimeActor())) ! StartMsg()
  }

  private def iaIsBlack(iaPlayer: Player): Boolean = iaPlayer.equals(Player.Black)

  private def suicide(): Unit = self ! PoisonPill

}