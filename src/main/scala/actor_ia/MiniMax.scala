package actor_ia

import akka.actor.{Actor, ActorRef, Props}
import ia.EvaluationFunction
import model.{ParserProlog, Piece, Player}
import utils.BoardGame.BoardCell
import utils.Coordinate

case class HeuristicValueMsg(score:Int)

case class MaxMinValueMsg(value: Int)

case class PruningAlfaBetaMsg()

case class TempValMsg(temp:Int)

abstract class MiniMax(game: ParserProlog, depth: Int, alfa: Int, beta: Int, move: (Coordinate, Coordinate), evaluationFunction: EvaluationFunction)  extends Actor {

  var numberOfChildren: Int = _

  def receive(): Receive = {


    case event: MaxMinValueMsg => miniMax(event.value)

    case _ : PruningAlfaBetaMsg =>  pruningAlfaBeta()


  }

  def createChild(game: ParserProlog, pawnMove: (Coordinate, Coordinate)): Props

  def analyzeMyChildren() : Unit = {

    val sonGame: ParserProlog = moveAnyPawn(game, move._1, move._2)

    val gamePossibleMove = game.gamePossibleMoves()

    numberOfChildren = gamePossibleMove.size

    var listSonRef:List[ActorRef] = List()

    for(pawnMove <- gamePossibleMove ){

        val sonActor = createChild(sonGame, pawnMove)
        val sonRef =  context.actorOf(sonActor)
        listSonRef = listSonRef :+ sonRef


    }

    println("Min: " + listSonRef.size + " Depth " + depth )


    listSonRef.foreach(ref => ref!PruningAlfaBetaMsg())

  }

  def miniMax(score: Int) : Unit

  def computeEvaluationFunction(): Unit

  def pruningAlfaBeta(): Unit =  depth match {
    case _ if isTerminalNode(game) => computeEvaluationFunction()
    case 0  => computeEvaluationFunction()
    case _ =>  analyzeMyChildren()
  }


  def findPlayerPawns(parserProlog: ParserProlog): List[Coordinate] = {
    val (player, boardCells ) = getPlayerAndBoard(parserProlog)
    for {
      cell <- boardCells
      if isOwner(cell.getPiece, player)
    } yield cell.getCoordinate
  }

  def getPlayerAndBoard(parserProlog: ParserProlog ):(Player.Value, List[BoardCell] ) = {
    (parserProlog.getPlayer, parserProlog.getActualBoard.cells.toList)
  }

  def isOwner(pawn:Piece.Value, player: Player.Value): Boolean = ( pawn, player) match {
    case ( Piece.WhitePawn, Player.White) => true
    case ( Piece.WhiteKing, Player.White) => true
    case ( Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

  def getGamePossibleMoves(parserProlog: ParserProlog, pawnsPlayer: List[Coordinate], gamePossibleMoves: List[(Coordinate,Coordinate) ] ) : List[(Coordinate,Coordinate) ] = pawnsPlayer match {
    case Nil => gamePossibleMoves
    case h::t => getGamePossibleMoves(parserProlog, t, getPawnPossibleMovesMap(h, parserProlog) ++ gamePossibleMoves )
  }

  def getPawnPossibleMovesMap( pawnCord: Coordinate, parserProlog: ParserProlog): List[(Coordinate,Coordinate)] = {
    getPawnPossibleMove(pawnCord, parserProlog).map( endCord => (pawnCord,endCord ))
  }

  def getPawnPossibleMove(coordinate: Coordinate, parserProlog: ParserProlog): List[Coordinate] = {
    parserProlog.showPossibleCells(coordinate).toList
  }

  def isTerminalNode(game:ParserProlog):Boolean = game.hasWinner.nonEmpty

  def  moveAnyPawn(parserProlog: ParserProlog, startCord: Coordinate, endCord: Coordinate ): ParserProlog = {
      val copy = parserProlog.copy()
      copy.makeLegitMove(startCord,endCord)
      copy
  }

}
