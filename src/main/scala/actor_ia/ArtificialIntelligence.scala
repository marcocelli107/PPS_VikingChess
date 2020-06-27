package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import model._
import utils.Move

import scala.collection.mutable

case class NewValueSonMsg(score: Int, actorRef: ActorRef)

case class ReturnBestMoveMsg(bestMove: Move)

trait ArtificialIntelligence {

  def findBestMove(gameSnapshot: GameSnapshot)

}

object ArtificialIntelligenceImpl {
  def apply(depth: Int): ArtificialIntelligenceImpl = {
    val myContext: ActorSystem = ActorSystem()
    val ia: ArtificialIntelligenceImpl = ArtificialIntelligenceImpl(depth)
    myContext.actorOf(Props(ia))
    ia
  }
}

case class ArtificialIntelligenceImpl(depth:Int) extends Actor with ArtificialIntelligence {

  val moveGenerator: MoveGenerator = MoveGenerator()
  private val hashMapSonRef: mutable.HashMap[ActorRef,Move] = mutable.HashMap.empty
  private var numberChildren: Int = 0
  private var bestMove: Move = _
  private var bestScore: Int = _

  override def findBestMove(gameSnapshot: GameSnapshot): Unit = {
    val gamePossibleMoves = moveGenerator.gamePossibleMoves(gameSnapshot)
    numberChildren = gamePossibleMoves.size

    for(possibleMove <- gamePossibleMoves) {
      val sonActor: Props = Props(MinActor(gameSnapshot.getCopy, depth, -100, 100, Option(possibleMove), self))
      val refSonActor = context.actorOf(sonActor)
      refSonActor ! StartMsg()
      hashMapSonRef += (refSonActor -> possibleMove)
    }
  }

  private def updateBest(newScore: Int, actorRef: ActorRef): Unit = {
    numberChildren -= 1
    bestScore = newScore
    bestMove = hashMapSonRef(actorRef)
    checkChildren()
  }

  private def checkChildren(): Unit = if(numberChildren == 0) self ! ReturnBestMoveMsg(bestMove)

  override def receive: Receive = {
    case event: NewValueSonMsg if event.score > bestScore => updateBest(event.score, event.actorRef)
    case event: ReturnBestMoveMsg => println(event.bestMove)
  }
}

object TryIA extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Tawlbwrdd.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Tawlbwrdd, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)

  val rootActor = ArtificialIntelligenceImpl(2)
  rootActor.findBestMove(gameSnapshot.getCopy)

}