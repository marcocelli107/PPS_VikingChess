package actor_ia

import akka.actor.{Actor, ActorSystem, Props}
import ia.EvaluationFunctionImpl
import model._

case class Alfa_Beta(game:ParserProlog, depth: Int, alfa: Int , beta : Int , phase: MaxMin.Value, playerIA: Player.Value)

case class MiniMaxAlghoritm() extends Actor {


  override def receive: Receive = {

    case event: Alfa_Beta if event.phase.equals(MaxMin.Max) => {

      event.game.gamePossibleMoves().foreach(move =>
                context.actorOf(Props(MaximizationActor(event.game, event.depth, event.alfa, event.beta, move, EvaluationFunctionImpl(7), self))) ! PruningAlfaBetaMsg())

    }
    case event: MaxMinValueMsg => context.children.foreach(child => context.stop(child));
      println(event.value);

  }


}

object TryMiniMax extends App{
  val system: ActorSystem = ActorSystem()
  val root = system.actorOf( Props(new MiniMaxAlghoritm), "Root")
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val board = game.createGame(GameVariant.Brandubh.nameVariant.toLowerCase)._3
  var ef: EvaluationFunctionImpl = new  EvaluationFunctionImpl(11)

  root! Alfa_Beta(game, 2, -100, 100, MaxMin.Max, Player.Black  )


}
