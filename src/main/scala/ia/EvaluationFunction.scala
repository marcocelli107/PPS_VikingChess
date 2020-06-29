package ia


import actor_ia.MoveGenerator
import ia.EvaluationFunctionImpl.EvaluationFunctionImpl
import model._
import utils.BoardGame.BoardCell
import utils.{BoardGame, Coordinate, Move}

import scala.util.Random

trait EvaluationFunction {
  def score(gameSnapshot: GameSnapshot, move: Move = null):Int
}

object EvaluationFunctionImpl {

  def apply(): EvaluationFunction = EvaluationFunctionImpl()

  case class EvaluationFunctionImpl() extends EvaluationFunction {

    private var boardSize: Int = _
    private var kingCoord: Coordinate = _
    private var cornerCells: Seq[Coordinate] = _
    private var quadrants: Seq[Seq[Seq[BoardCell]]]= _
    private var ortogonalKingCell: Seq[Seq[BoardCell]] = _
    private var blackCoord: Seq[Coordinate] = _



    override def score(gameSnapshot: GameSnapshot, moves: Move = null): Int = ((Random.nextDouble() * 200) - 100).intValue()

    def usefulValues(board: BoardGame.Board): Unit = {
      boardSize = board.size
      kingCoord = findKing(board.rows)
      ortogonalKingCell = board.orthogonalCells(kingCoord)
      cornerCells = board.cornerCoordinates
      quadrants = splitMatrixInFourPart(board.rows)
      blackCoord = board.rows.flatten.filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList

    }

    def computeScore(gameSnapshot: GameSnapshot):Int = {
      usefulValues(gameSnapshot.getBoard)
      compiuteBlackScore(gameSnapshot)  - compiuteWhiteScore(gameSnapshot) + scoreWinner(gameSnapshot)}



    def compiuteWhiteScore(gameSnapshot: GameSnapshot): Int = {
      scoreKingNearCorners() +
      scoreKingIsInFreeRowOrColumn() +
      scoreKingMovesToAFreeCorner() +
      scoreCapturedBlack(gameSnapshot)
    }


      def compiuteBlackScore(gameSnapshot: GameSnapshot): Int ={
      scoreBlackPawnProtectTheCorner()// +
      //scoreBlackSurroundTheKing() +
      //scoreCapturedWhite(gameSnapshot)
        }

    /* RULES */

    /*
    * White positive score
    * */

    def scoreCapturedBlack(gameSnapshot: GameSnapshot):Int = gameSnapshot.getNumberCapturedBlacks * 10

    // Positive score if king is near corner
    def scoreKingNearCorners(): Int = if (isItDistantFromCornerOf(kingCoord, 1)) 900 else 0

    // Positive score if king is in free row or column
    def scoreKingIsInFreeRowOrColumn(): Int = {
      val rowWhithoutKing = ortogonalKingCell(1) ++ ortogonalKingCell(3)
      val columnWhithoutKing = ortogonalKingCell(0) ++ ortogonalKingCell(2)

      def _scoreKingIsInFreeRowOrColumn(): Int = (rowWhithoutKing, columnWhithoutKing) match {
        case (row, column) if isSequenceFreeCells(row) && isSequenceFreeCells(column) => 50
        case (row, _) if isSequenceFreeCells(row) => 25
        case (_, column) if isSequenceFreeCells(column) => 25
        case _ => 0
      }

      _scoreKingIsInFreeRowOrColumn()
    }

    // Positive score if king moves to a free corner.
    // The score is inversely proportional to the concentration of black pawns in the king's quadrant.
    def scoreKingMovesToAFreeCorner(): Int = {
      val quadranKing = findQuadrant(kingCoord)
      val numberBlackPieceInKingQuadrant = quadranKing.flatten.count(cell => cell.getPiece.equals(Piece.BlackPawn))
      val score: Double = 100 * 1 / numberBlackPieceInKingQuadrant
      score.toInt
    }

    // Positive score if the white opening in opposite side of black Blockade
    def scoreWhiteOpeningOnOppositeSideOfBlackBlockade(whiteCoord: Coordinate): Int = {
      val oppositeQuadrant: Seq[Seq[BoardCell]] = findQuadrant(whiteCoord: Coordinate, true)
      val numberBlackPieceInOppositeKingQuadrant: Int = oppositeQuadrant.flatten.count(cell => cell.getPiece.equals(Piece.BlackPawn))
      val score: Int = numberBlackPieceInOppositeKingQuadrant * 2
      score
    }

    /*
    * Black score
    * */
    def scoreCapturedWhite(gameSnapshot: GameSnapshot):Int = gameSnapshot.getNumberCapturedWhites * 10

    //Positive score if the black pieces protect the corners - 	black barricade
    def scoreBlackPawnProtectTheCorner(): Int = {
      def scoreBlackPawn(pawnCoord: Coordinate): Double = quadraticDistanceBetweenCells(findCloserCorner(pawnCoord), pawnCoord) match {
        case 1 => -200
        case distance =>  100 * 1/distance
      }
      blackCoord.map(scoreBlackPawn(_)).sum.toInt

    }

    //Positive score if the black pieces Surround The King
    def scoreBlackSurroundTheKing(): Int = {
      def scoreBlackPawn(pawnCoord: Coordinate):Double = 100 * 1 / quadraticDistanceBetweenCells(pawnCoord, kingCoord)
      blackCoord.map(scoreBlackPawn(_)).sum.toInt
    }

    //Positive score if the pawn is diagonal alignment with other pawns
    def scorePawnDiagonalAlignment(pawnCell: BoardCell, closeCell: Seq[BoardCell], score: Int = 0): Int = ???

    /*
    * Jolly rules
    * */

    //Positive score if pieces (black or white) arranged in square
    def scorePawnArrangedInSquare(): Double = ???

    def scoreWinner(snapshot: GameSnapshot): Int = snapshot.getWinner match {
      case Player.Black => 100
      case Player.White => -100
      case _ => 0
    }


    /* UTILS METHODS */


    def findQuadrant(coord: Coordinate, oppositQuadrant: Boolean = false): Seq[Seq[BoardCell]] = {

      def getQuadrant(coord: Coordinate): Int = {
        if (isInRange(coord, (1, boardSize / 2), (1, boardSize / 2))) 0
        else if (isInRange(coord, (boardSize / 2 + 1, boardSize), (1, boardSize / 2))) 1
        else if (isInRange(coord, (boardSize / 2 + 1, boardSize), (1, boardSize / 2))) 2
        else 3
      }

      def opposite(indexQuadrant: Int): Int = indexQuadrant match {
        case 0 => 3
        case 3 => 0
        case 1 => 2
        case _ => 1
      }

      def _findQuadrant(index: Int, quadrants: Seq[Seq[Seq[BoardCell]]], oppositQuadrant: Boolean): Seq[Seq[BoardCell]] =
        if (oppositQuadrant)
          quadrants(opposite(index))
        else
          quadrants(index)

      _findQuadrant(getQuadrant(coord), quadrants, oppositQuadrant)
    }


    def isInRange(coordinate: Coordinate, rangeX: (Int, Int), rangeY: (Int, Int)): Boolean = coordinate match {
      case Coordinate(x, y) if x >= rangeX._1 && x <= rangeX._2 && y >= rangeY._1 && y <= rangeY._2 => true
      case _ => false
    }


    def isItDistantFromCornerOf(coord: Coordinate, distance: Int): Boolean = {

      def _isItDistantFromTheCornerOf(coord: Coordinate, distance: Int, cornerCoord: List[Coordinate]): Boolean = cornerCoord match {
        case Nil => false
        case h :: _ if quadraticDistanceBetweenCells(h, coord) == distance => true
        case _ :: t => _isItDistantFromTheCornerOf(coord, distance, t)
      }

      _isItDistantFromTheCornerOf(coord, distance, getCoordCorners)
    }

    def transposeCoordinates(coord: Coordinate): Coordinate = coord match {
      case Coordinate(x, y) => Coordinate(y, x)
    }

    def whithoutKing(seq: Seq[BoardCell]): Seq[BoardCell] = seq.filter(cell => !cell.getPiece.equals(Piece.WhiteKing))

    //Quadratic distance
    def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate): Int = (scala.math.pow(start.x - end.x, 2) + scala.math.pow(start.y - end.y, 2)).toInt

    def findCloserCorner(coord: Coordinate): Coordinate = {
      def _getCloserCorner( closerCornerCord: Coordinate, closerDist: Int, cells: Seq[Coordinate] = cornerCells ): Coordinate = cells match {
        case Nil => closerCornerCord
        case h :: t =>
          val distCoordFromCorner = quadraticDistanceBetweenCells(h, coord)
          val (newCloserCorner, newCloserDist) = if (distCoordFromCorner > closerDist) (closerCornerCord, closerDist) else (h, distCoordFromCorner)
          _getCloserCorner( newCloserCorner, newCloserDist,t)
      }

      _getCloserCorner(Coordinate(1,1), Int.MaxValue)
    }

    // Split a rows sequence in dials sequence
    def splitMatrixInFourPart(seqRows: Seq[Seq[BoardCell]]): Seq[Seq[Seq[BoardCell]]] = {
      val sizeSplit: Int = boardSize / 2
      val northAndSouth = seqRows.toList.splitAt(sizeSplit)

      def splitList(seq: Seq[Seq[BoardCell]], westSeq: Seq[Seq[BoardCell]], estSeq: Seq[Seq[BoardCell]]): Seq[Seq[Seq[BoardCell]]]= seq match {
        case h :: t =>
          val westAndEst = h.splitAt(sizeSplit)
          splitList(t, westSeq :+ westAndEst._1, estSeq :+ westAndEst._2)

        case _ => Seq(westSeq,estSeq)
      }

      val firstAndSecond = splitList(northAndSouth._1, Seq(), Seq())
      val thirdAndQuart = splitList(northAndSouth._2, Seq(), Seq())

      firstAndSecond ++ thirdAndQuart

    }


    def isSequenceFreeCells(seq: Seq[BoardCell]): Boolean =
      seq.count(boardCell => !boardCell.getPiece.equals(Piece.Empty)) == 0

    def getCoordCorners = List(Coordinate(1, 1), Coordinate(1, boardSize), Coordinate(boardSize, 1), Coordinate(boardSize, boardSize))


    def findKing(board: Seq[Seq[BoardCell]]): Coordinate =
      board.flatten.filter(_.getPiece.equals(Piece.WhiteKing)).map(_.getCoordinate).head
  }

}

object blabla extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Hnefatafl.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Hnefatafl, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)
  val ef: EvaluationFunctionImpl = new EvaluationFunctionImpl()
  val mg:MoveGenerator= MoveGenerator()

  val newGs = mg.makeMove(gameSnapshot, Move(Coordinate(6,6), Coordinate(4,3)))

  println(ef.score(newGs))



}