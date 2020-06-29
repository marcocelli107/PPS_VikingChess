package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import model._
import utils.Move

import scala.collection.mutable


case class ReturnBestMoveMsg(bestMove: Move)

case class FindBestMoveMsg(gameSnapshot: GameSnapshot)

object ArtificialIntelligenceImpl {
  def apply(model: ModelHnefatafl, depth: Int, playerChosen: Player.Value): ArtificialIntelligenceImpl = new ArtificialIntelligenceImpl(model, depth, playerChosen)
}

case class ArtificialIntelligenceImpl(model: ModelHnefatafl, depth: Int, playerChosen: Player.Value) extends Actor {

  private val hashMapSonRef: mutable.HashMap[ActorRef, Move] = mutable.HashMap.empty
  private var numberChildren: Int = 0
  private var bestMove: Move = _
  private var bestScore: Int = _

  def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    bestScore = if (iaIsBlack()) Int.MaxValue else Int.MinValue
    val gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
    numberChildren = gamePossibleMoves.size

    for (possibleMove <- gamePossibleMoves) {
      var sonActor: Props = Props.empty
      if (!iaIsBlack())
        sonActor = Props(MinActor(gameSnapshot.getCopy, depth - 1, Option(possibleMove), self,0))
      else
        sonActor = Props(MaxActor(gameSnapshot.getCopy, depth - 1, Option(possibleMove), self,0))
      val refSonActor = context.actorOf(sonActor)
      refSonActor ! StartMsg()
      hashMapSonRef += (refSonActor -> possibleMove)
    }
  }

  private def iaIsBlack(): Boolean = playerChosen.equals(Player.White)

  private def updateBest(newScore: Int, actorRef: ActorRef): Unit = {
    //println("NewScore " + newScore)
    numberChildren -= 1
    if (newScore > bestScore && !iaIsBlack() ) {
      bestScore = newScore
      bestMove = hashMapSonRef(actorRef)
    } else if( newScore < bestScore && iaIsBlack())  {
      bestScore = newScore
      bestMove = hashMapSonRef(actorRef)
    }
    checkChildren()
  }

  private def checkChildren(): Unit = if (numberChildren == 0) self ! ReturnBestMoveMsg(bestMove)

  override def receive: Receive = {
    case event: FindBestMoveMsg => findBestMove(event.gameSnapshot)
    case event: ValueSonMsg => updateBest(event.score, context.sender())
    case _: ReturnBestMoveMsg => model.makeMove(bestMove)
  }
}

object TryIA extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Tawlbwrdd.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Tawlbwrdd, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)
  val system: ActorSystem = ActorSystem()

  system.actorOf(Props(ArtificialIntelligenceImpl(null, 2, gameSnapshot.getPlayerToMove))) ! FindBestMoveMsg(gameSnapshot)


}