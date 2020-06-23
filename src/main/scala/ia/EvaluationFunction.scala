package ia


import model.{ParserProlog, Piece, Player}
import utils.BoardGame.{Board, BoardCell}
import utils.Coordinate

import scala.util.Random

trait EvaluationFunction{
  def score(gameState: ParserProlog):Int
}

class  EvaluationFunctionImpl( boardSize: Int ) extends EvaluationFunction {


  override def score(gameState: ParserProlog): Int =  ((Random.nextDouble() * 200) -100).intValue()

  /* RULES */

  /*
  * White positive score
  * */

  // Positive score if king is near corner
  def scoreKingNearCorners( kingCoord: Coordinate): Int = isItDistantFromCornerOf(kingCoord,1) match {
    case true  => 90
    case false => 0
  }

  // Positive score if king is in free row or column
  def scoreKingIsInFreeRowOrColumn( rowKing : Seq [BoardCell], columnKing: Seq [BoardCell] ): Int = {
     val rowWhithoutKing = whithoutKing(rowKing)
     val columnWhithoutKing = whithoutKing(columnKing)

      def _scoreKingIsInFreeRowOrColumn(rowKing : Seq [BoardCell], columnKing: Seq [BoardCell] ): Int = (rowKing,columnKing) match {
        case ( row , column ) if isSequenceFreeCells(row) && isSequenceFreeCells(column)   => 10
        case (row, _ ) if isSequenceFreeCells(row) => 5
        case ( _, column ) if isSequenceFreeCells(column) => 5
        case _ => 0
      }

    _scoreKingIsInFreeRowOrColumn(rowWhithoutKing,columnWhithoutKing)
  }

  // Positive score the white opening in opposite side of black Blockade
  def scoreWhiteOpeningOnOppositeSideOfBlackBlockade(whiteCoord: Coordinate): Int = ???

  /*
  * Black positive score
  * */

  //Positive score if the black pieces protect the corners - 	black barricade
  def scoreBlackPawnProtectTheCorner( pawnBlackCoord: Coordinate): Int =  {
    val closerCorner = getCloserCorner(pawnBlackCoord)
    val score: Int =  20 * 1 / quadraticDistanceBetweenCells(closerCorner, pawnBlackCoord )
    score
  }

  //Positive score if the black pieces Surround The King
  def scoreBlackSurroundTheKing( pawnCoord: Coordinate, board: Seq[BoardCell]): Int = {
    val kingCoord: Coordinate = findKing(board)
    val score: Int =  10 * 1 / quadraticDistanceBetweenCells(pawnCoord, kingCoord )
    score
  }

  //Positive score if the pawn is diagonal alignment with other pawns
  def scorePawnDiagonalAlignment(pawnCell: BoardCell, closeCell: Seq[BoardCell], score: Int = 0): Int = closeCell match {
    case Nil => score
    case h::t  if h.getPiece.equals(pawnCell.getPiece) &&
                  quadraticDistanceBetweenCells(h.getCoordinate,pawnCell.getCoordinate  ) == 2 => scorePawnDiagonalAlignment(pawnCell, t, score + 5 )
  }


  /*
  * Jolly rules
  * */

  //Positive score if pieces (black or white) arranged in square
  def scorePawnArrangedInSquare(): Double = ???

  def scoreWinner(game: ParserProlog): Int  = game.hasWinner match {
    case Some( Player.Black) => 100
    case Some( Player.White) => -100
    case _ => 0
  }

  /* UTILS METHODS */

  def areAlignedDiagonally(pawnCell: BoardCell, cells: Seq[BoardCell]):Boolean = {
    var mapCells:Seq[(BoardCell,BoardCell)] = cells.sliding(2).toList.map(listBuffer => (listBuffer(0), listBuffer(1)))

    def _areAlignedDiagonally(pawnCell: BoardCell, mapCells: Seq[(BoardCell,BoardCell)] ):Boolean = mapCells match {
      case Nil => true
      case h::_ if quadraticDistanceBetweenCells(h._1.getCoordinate, h._2.getCoordinate) == 2 &&
                    pawnCell.getPiece.equals()=> ???

    }

    _areAlignedDiagonally(pawnCell,mapCells)
  }

  def isItDistantFromCornerOf(coord:Coordinate, distance: Int):Boolean = {

    def _isItDistantFromTheCornerOf(coord:Coordinate, distance: Int, cornerCoord: List[Coordinate]): Boolean = cornerCoord match {
      case Nil => false
      case h::_ if(quadraticDistanceBetweenCells(h,coord) == distance) => true
      case _::t  => _isItDistantFromTheCornerOf(coord, distance, t )
    }
    _isItDistantFromTheCornerOf(coord, distance,getCoordCorners() )
  }

  def transposeCoordinates( coord :Coordinate): Coordinate = coord match {
    case Coordinate(x,y) => Coordinate(y,x)
  }

  def whithoutKing( seq : Seq [BoardCell]): Seq [BoardCell] = seq.filter( cell => !cell.getPiece.equals(Piece.WhiteKing))

  //Quadratic distance
  def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate ): Int =  (scala.math.pow(start.x - end.x,2)+scala.math.pow(start.y - end.y,2)).toInt

  def getCloserCorner(coord:Coordinate):Coordinate = {
    def _getCloserCorner(coord:Coordinate, cornerCoord: List[Coordinate], closerCornerCord: Coordinate, closerDist: Int ): Coordinate = cornerCoord match {
      case Nil => closerCornerCord
      case h::t => {
            val distCoordFromCorner = quadraticDistanceBetweenCells(h,coord)
            val (newCloserCorner, newCloserDist) = if( distCoordFromCorner > closerDist ) (closerCornerCord, closerDist) else (h,distCoordFromCorner )
            _getCloserCorner(coord,t,newCloserCorner,newCloserDist)
      }
    }
    _getCloserCorner(coord, getCoordCorners(), null, 20)
  }

  def getSeqRows(board: Board): Seq[Seq[BoardCell]] = {
    board.cells.grouped(boardSize).toSeq
  }

  def getSeqColumns(board: Board): Seq[Seq[BoardCell]] = {
    getSeqRows(board).transpose
  }

  // Split a rows sequence in dials sequence
  def spltMatrixInFourPart(seqRows:Seq[Seq[BoardCell]]): Seq[Seq[BoardCell]] = {
    val sizeSplit:Int = boardSize / 2
    val northAndSouth = seqRows.toList.splitAt(sizeSplit)

      def splitList(seq:Seq[Seq[BoardCell]], westSeq: Seq[Seq[BoardCell]], estSeq: Seq[Seq[BoardCell]]):Seq[Seq[BoardCell]] = seq match {
        case h::t  => {
          val westAndEst = h.splitAt(sizeSplit)
          splitList( t,  westSeq:+ westAndEst._1 , estSeq :+  westAndEst._2  )
        }
        case _  => westSeq ++ estSeq
      }
      val firstAndSecond = splitList(northAndSouth._1, Seq(), Seq())
      val thirdAndQuart = splitList(northAndSouth._2, Seq(), Seq())

    firstAndSecond ++ thirdAndQuart

  }

  def getRow(row:Int , cells:Seq[BoardCell]): Seq[BoardCell] = cells.filter(cell => cell.getCoordinate.y.equals(row))

  def getColumn(column:Int, cells:Seq[BoardCell] ): Seq[BoardCell] = cells.filter(cell => cell.getCoordinate.x.equals(column))

  def isSequenceFreeCells(seq: Seq[BoardCell] ):Boolean = {
    seq.filter(boardCell => !boardCell.getPiece.equals(Piece.Empty)).size == 0
  }

  def getCoordCorners() = List(Coordinate(1,1),Coordinate(1,boardSize),Coordinate(boardSize,1),Coordinate(boardSize,boardSize))

  def findKing(board: Seq[BoardCell]):Coordinate= {
    board.filter(cell=> cell.getPiece.equals( Piece.WhiteKing))
      .map(cell=> cell.getCoordinate)
      .head}

}

object EvaluationFunctionImpl {

  def apply(boardSize: Int): EvaluationFunction = new EvaluationFunctionImpl(boardSize: Int)
}