package actor_ia

import akka.actor.{ActorRef, Props}
import ia.EvaluationFunction
import model.ParserProlog
import utils.Coordinate

case class MinimizationActor (game: ParserProlog, depth: Int, var alfa: Int, var beta: Int , evaluationFunction: EvaluationFunction, fatherMove: (Coordinate,Coordinate), fatherRef: ActorRef ) extends MiniMax(game, depth,alfa,beta,evaluationFunction){

  var myAlfa:Int = alfa
  var myBeta:Int = beta
  var tempVal:Int = 100
  var numberOfChildren = 0

  override def analyzeMyChildren(): Unit = {


    val gamePossibleMove = game.gamePossibleMoves()

    numberOfChildren = gamePossibleMove.size

    var listSonRef:List[ActorRef] = List()

    for(pawnMove <- gamePossibleMove ){

      try {
        val sonGame: ParserProlog = moveAnyPawn(game, pawnMove._1, pawnMove._2)
        val sonActor: Props = Props(MaximizationActor( sonGame, depth-1, myAlfa, myBeta, evaluationFunction, self))
        val sonRef =  context.actorOf(sonActor)
        listSonRef = listSonRef :+ sonRef
      } catch {
        case _  =>  println("Move Coord" + pawnMove + "Board " + game.getActualBoard + " Winner "+ game.hasWinner + " Player"+ game.getPlayer + " Father Move " + fatherMove)
      }


    }

    //println("Min: " + listSonRef.size + " Depth " + depth  )


    //listSonRef.foreach(ref => ref!PruningAlfaBetaMsg())
  }

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
    println(v)
      fatherRef! MaxMinValueMsg(v)
  }
}
