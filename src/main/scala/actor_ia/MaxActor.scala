package actor_ia

import akka.actor.{ActorRef, Props}
import model.ParserProlog
import utils.Coordinate

case class MaxActor(game: ParserProlog, depth:Int, alfa: Int, beta: Int, move: (Coordinate,Coordinate), fatherRef: ActorRef) extends MiniMaxActor(game, depth, move, fatherRef) {

  tempVal = -100
  myAlfa = alfa
  myBeta = beta

  override def createChild(fatherGame: ParserProlog, move: (Coordinate, Coordinate), fatherRef: ActorRef): Props =
    Props(MinActor(fatherGame, depth-1, myAlfa, myBeta, move, fatherRef))

  override def miniMaxComparison(score: Int): Unit = {
    tempVal = scala.math.max(tempVal, score)
    myAlfa = scala.math.max(tempVal, myAlfa)
  }

}
