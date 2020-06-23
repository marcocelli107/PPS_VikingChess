package actor_ia

import akka.actor.{ActorRef, Props}
import model.ParserProlog
import utils.Coordinate

case class MinActor(game: ParserProlog, depth:Int, var alfa: Int, var beta: Int, move: (Coordinate,Coordinate), fatherRef: ActorRef) extends MiniMaxActor(game, depth, move, fatherRef) {

  tempVal = 100
  myAlfa = alfa
  myBeta = beta

  override def createChild(fatherGame: ParserProlog, move: (Coordinate, Coordinate), fatherRef: ActorRef): Props =
    Props(MaxActor(fatherGame, depth-1, alfa, beta, move, fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    tempVal = scala.math.min(tempVal, score)
    beta = scala.math.min(tempVal, beta)
  }
}
