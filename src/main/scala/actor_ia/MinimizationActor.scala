package actor_ia

import akka.actor.{ActorRef, Props}
import ia.EvaluationFunction
import model.ParserProlog
import utils.Coordinate

case class MinimizationActor (game: ParserProlog, depth: Int, var alfa: Int, var beta: Int ,move: (Coordinate, Coordinate), evaluationFunction: EvaluationFunction, fatherRef: ActorRef ) extends MiniMax(game, depth, alfa, beta, move, evaluationFunction){

  var myAlfa:Int = alfa
  var myBeta:Int = beta
  var tempVal:Int = 100

  override def createChild(sonGame: ParserProlog, move: (Coordinate, Coordinate)): Props =
    Props(MaximizationActor( sonGame, depth-1, myAlfa, myBeta, move, evaluationFunction, self))

  override def miniMax(score: Int): Unit = {
    numberOfChildren = numberOfChildren - 1
    tempVal = scala.math.min(tempVal, score)
    myBeta = scala.math.min(tempVal, myBeta)

    if( myBeta <= myAlfa || numberOfChildren == 0) {
      context.children.foreach( child => context.stop(child))
      fatherRef! MaxMinValueMsg(tempVal)
    }

  }

  override def computeEvaluationFunction(): Unit = {
    val v= evaluationFunction.score(game)
     fatherRef! MaxMinValueMsg(v)
  }
}