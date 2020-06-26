package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

case class MaxActor(gameSnapshot: GameSnapshot, depth:Int, alfa: Int, beta: Int, move: Move, fatherRef: ActorRef) extends MiniMaxActor(gameSnapshot, depth, move, fatherRef) {

  tempVal = -100
  myAlfa = alfa
  myBeta = beta

  override def createChild(fatherGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MinActor(fatherGame, depth-1, myAlfa, myBeta, move, fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    tempVal = scala.math.max(tempVal, score)
    myAlfa = scala.math.max(tempVal, myAlfa)
  }

}
