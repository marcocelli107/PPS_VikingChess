package actor_ia

import akka.actor.{Actor, ActorRef, Props}
import model.GameSnapshot
import utils.Move

import scala.collection.mutable.HashMap

trait ArtificialIntelligence{

  def findBestMove(gameSnapshot: GameSnapshot)
}

object ArtificialIntelligenceImpl{
  def apply(depth: Int): ArtificialIntelligenceImpl = new ArtificialIntelligenceImpl(depth)
}

class ArtificialIntelligenceImpl(depth:Int) extends Actor with ArtificialIntelligence {

  val moveGenerator: MoveGenerator = MoveGenerator()
  private val hashMapSonRef: HashMap[ActorRef,Move] = HashMap()
  private val numberChildre: Int = 0

  override def findBestMove(gameSnapshot: GameSnapshot)= {
    val gamePossibleMove = moveGenerator.gamePossibleMoves(gameSnapshot)

    for(pawnMove <- gamePossibleMove) {
      val sonActor: Props = Props(MinActor(gameSnapshot.getCopy, depth, -100, 100, pawnMove, this.self ))
      hashMapSonRef += (context.actorOf(sonActor) -> pawnMove)
    }
  }

  override def receive: Receive = ???
}
