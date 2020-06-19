package ia


import model.{ParserProlog, Piece}
import utils.BoardGame.{Board, BoardCell}
import utils.Coordinate

import scala.util.Random

trait EvaluationFunction{
  def score(gameState: ParserProlog):Int
}

class  EvaluationFunctionImpl() extends EvaluationFunction {


  override def score(gameState: ParserProlog): Int =  ((Random.nextDouble() * 200) -100).intValue()

  /* RULES */

  def scoreKingNearCorners(size: Int, kingCoord: Coordinate): Int = kingCoord match {
    case  Coordinate(1,2) => 10
    case  Coordinate(2,1) => 10
    case  Coordinate(1,x) if (size - x) == 1 => 10
    case  Coordinate(2,x) if size == x => 10
    case  Coordinate(x,1) if (size - x) == 1 => 10
    case  Coordinate(x,2) if size == x => 10
    case  Coordinate(x,y) if ((size-x) == 1  && y == size ) => 10
    case  Coordinate(x,y) if ((size-y) == 1 && x == size ) => 10
    case  _ => 0
  }

  def scoreKingIsInFreeRowOrColumn( rowKing : Seq [BoardCell], columnKing: Seq [BoardCell] ): Int = {
     val rowWhithoutKing = whithoutKing(rowKing)
     val columnWhithoutKing = whithoutKing(columnKing)

      def _scoreKingIsInFreeRowOrColumn(rowKing : Seq [BoardCell], columnKing: Seq [BoardCell] ): Int =(rowKing,columnKing) match {
        case (row, _ ) if isSequenceFreeCells(row) => 5
        case ( _, column ) if isSequenceFreeCells(column) => 5
        case ( row , column ) if isSequenceFreeCells(row) && isSequenceFreeCells(column)   => 10
        case _ => 0
      }
    _scoreKingIsInFreeRowOrColumn(rowWhithoutKing,columnWhithoutKing)
  }

  def scorePawnArrangedInSquare(): Double = ???

  def scoreCapturePawns(): Double  = ???

  /* UTILS METHODS */

  def whithoutKing( seq : Seq [BoardCell]): Seq [BoardCell] = seq.filter( cell => !cell.getPiece.equals(Piece.WhiteKing))

  def distanceBetweenCells(start: Coordinate, end: Coordinate ): Double= scala.math.sqrt(scala.math.sqrt(start.x - end.y)+scala.math.sqrt(start.y - end.y))

  def getSeqRows(board: Board): Seq[Seq[BoardCell]] = {
    board.cells.grouped(board.size).toSeq
  }

  def getSeqColumns(board: Board): Seq[Seq[BoardCell]] = {
    getSeqRows(board).transpose
  }

  def getRow(row:Int , cells:Seq[BoardCell]): Seq[BoardCell] = cells.filter(cell => cell.getCoordinate.y.equals(row))

  def getColumn(column:Int, cells:Seq[BoardCell] ): Seq[BoardCell] = cells.filter(cell => cell.getCoordinate.x.equals(column))

  def isSequenceFreeCells(seq: Seq[BoardCell] ):Boolean = {
    seq.filter(boardCell => !boardCell.getPiece.equals(Piece.Empty)).size == 0
  }

  def getCoordCorners(boardSize: Int) = List(Coordinate(1,1),Coordinate(1,boardSize),Coordinate(boardSize,1),Coordinate(boardSize,boardSize))

  def findKing(board: Seq[BoardCell]):Coordinate= {
    board.filter(cell=> cell.getPiece.equals( Piece.WhiteKing))
      .map(cell=> cell.getCoordinate)
      .head}

}

object EvaluationFunctionImpl {

  def apply(): EvaluationFunction = new EvaluationFunctionImpl()
}