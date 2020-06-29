package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

case class MaxActor(gameSnapshot: GameSnapshot, depth: Int,  move: Option[Move], fatherRef: ActorRef, count:Int ) extends MiniMaxActor(gameSnapshot, depth, move, fatherRef, count) {

  alfa = Int.MinValue


  override def createChild(fatherGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props =
    Props(MinActor(fatherGame, depth - 1,  Option(move), fatherRef, count+1))

  override def miniMaxComparison(score: Int): Unit = {
    alfa = scala.math.max(alfa, score)

  }
}
