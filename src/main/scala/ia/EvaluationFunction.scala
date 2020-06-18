package ia


import model.{ParserProlog, Piece}
import utils.BoardGame.{Board, BoardCell}
import utils.Coordinate

trait EvaluationFunction{
  def score(gameState: ParserProlog):Int
}

class  EvaluationFunctionImpl extends EvaluationFunction {


  override def score(gameState: ParserProlog): Int = ???


  /* RULES */

  def scoreKingNearCorners(board: Board): Double = {
    val kingCoord: Coordinate = findKing(board.cells)
    if( getCoordCorners(board.size).filter(coord => distanceBetweenCells(kingCoord,coord) == 1 ).size == 0 )  0 else 1

  }

  def scoreKingIsInFreeRowsOrColumns(cells: Seq[BoardCell]): Double = ??? /*{
     val coordKing:Coordinate = findKing(cells)
     val row: Seq[BoardCell] = getRow(coordKing.getX, cells).filter(cell => !cell.getCoordinate.equals(coordKing))
     val column: Seq[BoardCell] = getRow(coordKing.getY, cells).filter(cell => !cell.getCoordinate.equals(coordKing))
   }*/

  /* RULES */

  def scorePawnArrangedInSquare(): Double = ???

  def scoreCapturePawns(): Double  = ???

  /* UTILS METHODS */

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
    seq.filter(boardCell => !boardCell.getPiece.equals(Piece.Void)).size == 0
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