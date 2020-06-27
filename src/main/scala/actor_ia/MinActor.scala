package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

case class MinActor(fatherGameSnapshot: GameSnapshot, depth: Int, var alfa: Int, var beta: Int, move: Option[Move], fatherRef: ActorRef) extends MiniMaxActor(fatherGameSnapshot, depth, move, fatherRef) {

  tempVal = 100
  myAlfa = alfa
  myBeta = beta

  override def createChild(currentGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MaxActor(currentGame, depth - 1, myAlfa, myBeta, Option(move), fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    tempVal = scala.math.min(tempVal, score)
    myBeta = scala.math.min(tempVal, myBeta)
  }
}
