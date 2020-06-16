package ia


import model.{Piece, Player}
import utils.BoardGame.{Board, BoardCell}
import utils.Pair


trait EvaluationFunction{
  def score(board :Board, maxPlayer: Player.Value):Double

}

 class  EvaluationFunctionImpl extends EvaluationFunction {

   override def score(board: Board, maxPlayer: Player.Value): Double = ???

   def distanceBetweenCells(start: Pair[Int], end: Pair[Int] ): Double= scala.math.sqrt(scala.math.sqrt(start.getX-end.getY)+scala.math.sqrt(start.getY-end.getY))

   def getRows(board: Board): Seq[Seq[BoardCell]] = {
      board.cells.grouped(board.size).toSeq
    }

   def getColumns(board: Board): Seq[Seq[BoardCell]] = {
      getRows(board).transpose
   }

   def getCoordCorners(boardSize: Int) = List(Pair(1,1),Pair(1,boardSize),Pair(boardSize,1),Pair(boardSize,boardSize))

   def findKing(board: Seq[BoardCell]):Pair[Int]= {
              board.filter(cell=> cell.getPiece.equals( Piece.WhiteKing))
                   .map(cell=> cell.getCoordinate)
                   .head}

   /* RULE */

   def scoreKingNearCorners(board: Board): Double = {
     val kingCoord: Pair[Int]= findKing(board.cells)
     if( getCoordCorners(board.size).filter(coord => distanceBetweenCells(kingCoord,coord) == 1 ).size == 0 )  0 else 1

   }

   def scoreKingsInFreeRowsOrColumns(): Double = ???

   def scorePawnArrangedInSquare(): Double = ???

   def scoreCapturePawns(): Double  = ???

 }

object EvaluationFunction {

  def apply(): EvaluationFunction = new EvaluationFunctionImpl()
}
