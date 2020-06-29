package actor_ia

import akka.actor.{ActorRef, Props}
import model.{GameSnapshot, Player}
import utils.Move

case class MinActor(fatherGameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef) extends MiniMaxActor(fatherGameSnapshot, depth, move, fatherRef) {

  alfa = Int.MaxValue


  override def createChild(currentGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MaxActor(currentGame, depth - 1,  Option(move), fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    alfa = scala.math.min(alfa, score)

  }
}
