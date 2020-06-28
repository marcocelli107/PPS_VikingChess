package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import model._
import utils.Move

import scala.collection.mutable


case class ReturnBestMoveMsg(bestMove: Move)

case class FindBestMoveMsg(gameSnapshot: GameSnapshot)

object ArtificialIntelligenceImpl {
  def apply(model: ModelHnefatafl, depth: Int): ArtificialIntelligenceImpl = new ArtificialIntelligenceImpl(model, depth)
}

 case class ArtificialIntelligenceImpl(model: ModelHnefatafl, depth:Int) extends Actor  {

  val moveGenerator: MoveGenerator = MoveGenerator()
  private val hashMapSonRef: mutable.HashMap[ActorRef,Move] = mutable.HashMap.empty
  private var numberChildren: Int = 0
  private var bestMove: Move = _
  private var bestScore: Int = Int.MinValue

 def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    val gamePossibleMoves = moveGenerator.gamePossibleMoves(gameSnapshot)
    numberChildren = gamePossibleMoves.size

    for(possibleMove <- gamePossibleMoves) {
      val sonActor: Props = Props(MinActor(gameSnapshot.getCopy, depth, Option(possibleMove), self))
      val refSonActor = context.actorOf(sonActor)
      refSonActor ! StartMsg()
      hashMapSonRef += (refSonActor -> possibleMove)
    }
  }

  private def updateBest(newScore: Int, actorRef: ActorRef): Unit = {
    //println("NewScore " + newScore)
    numberChildren -= 1
    if (newScore > bestScore)
      bestScore = newScore
      bestMove = hashMapSonRef(actorRef)
    checkChildren()
  }

  private def checkChildren(): Unit = if(numberChildren == 0) self ! ReturnBestMoveMsg(bestMove)

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

  system.actorOf(Props(ArtificialIntelligenceImpl(null, 2)))!FindBestMoveMsg(gameSnapshot)





}