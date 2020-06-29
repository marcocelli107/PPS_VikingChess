package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

case class MinActor(fatherGameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef, count:Int) extends MiniMaxActor(fatherGameSnapshot, depth, move, fatherRef, count ) {

  alfa = Int.MaxValue


  override def createChild(currentGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MaxActor(currentGame, depth - 1,  Option(move), fatherRef, count+1))

  override def miniMaxComparison(score: Int): Unit = {
    alfa = scala.math.min(alfa, score)

  }
}
