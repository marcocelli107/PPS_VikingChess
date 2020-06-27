package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

case class MinActor(gameSnapshot: GameSnapshot, depth: Int, var alfa: Int, var beta: Int, move: Move, fatherRef: ActorRef) extends MiniMaxActor(gameSnapshot, depth, move, fatherRef) {

  tempVal = 100
  myAlfa = alfa
  myBeta = beta

  override def createChild(fatherGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MaxActor(fatherGame, depth - 1, myAlfa, myBeta, move, fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    tempVal = scala.math.min(tempVal, score)
    myBeta = scala.math.min(tempVal, myBeta)
  }
}
