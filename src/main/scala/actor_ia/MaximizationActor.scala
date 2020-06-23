package actor_ia

import akka.actor.{ActorRef, Props}
import ia.EvaluationFunction
import model.ParserProlog

case class MaximizationActor (game: ParserProlog, depth: Int, var alfa: Int, var beta: Int , evaluationFunction: EvaluationFunction,  fatherRef: ActorRef) extends MiniMax(game, depth,alfa,beta,evaluationFunction){

  var myAlfa:Int = alfa
  var myBeta:Int = beta
  var tempVal:Int = -100
  var numberOfChildren = 0

  override def analyzeMyChildren(): Unit = {

    val gamePossibleMove = game.gamePossibleMoves()

    numberOfChildren = gamePossibleMove.size
    var listSonRef:List[ActorRef] = List()

    for(pawnMove <- gamePossibleMove ){
      val sonGame: ParserProlog = moveAnyPawn(game, pawnMove._1, pawnMove._2)
      val sonActor: Props = Props (MinimizationActor ( sonGame, depth-1, myAlfa, myBeta, evaluationFunction,pawnMove, self))
      val sonRef =  context.actorOf(sonActor)
      listSonRef = listSonRef :+ sonRef

    }
   // println("MAX: " + listSonRef.size + "Depth " + depth)
    listSonRef.foreach(ref => ref!PruningAlfaBetaMsg())
  }

  override def miniMax(score: Int): Unit = {
      numberOfChildren = numberOfChildren - 1
      tempVal = scala.math.max(tempVal, score)
      myAlfa = scala.math.max(tempVal, myAlfa)
      if(myBeta <= myAlfa || numberOfChildren == 0) {
        context.children.foreach( child => context.stop(child))
        fatherRef! MaxMinValueMsg(tempVal)
      }

  }

  override def computeEvaluationFunction(): Unit =  fatherRef! MaxMinValueMsg(evaluationFunction.score(game))
}
