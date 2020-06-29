package ia


import actor_ia.MoveGenerator
import model._
import utils.BoardGame.BoardCell
import utils.{Coordinate, Move}

trait EvaluationFunction {
  def score(gameSnapshot: GameSnapshot, move: Move = null):Int
}

object EvaluationFunctionImpl {

  def apply(): EvaluationFunction = EvaluationFunctionImpl()

  case class EvaluationFunctionImpl() extends EvaluationFunction {

    private var boardSize: Int = _
    private var kingCoord: Coordinate = _
    private var cornerCoordinates: List[Coordinate] = _
    private var centralCoordinate: Coordinate = _
    private var quadrants: Seq[Seq[Seq[BoardCell]]]= _
    private var kingOrthogonalCells: Seq[Seq[BoardCell]] = _
    private var kingAdjacentCells: Seq[Option[BoardCell]] = _
    private var blackCoord: Seq[Coordinate] = _
    private var gamePossibleMoves: Seq[Move] = _

    override def score(snapshot: GameSnapshot, moves: Move = null): Int = {
      if(!snapshot.getWinner.equals(Player.None)) {
       // println("Board " + snapshot.getBoard)
        // println(snapshot.getWinner)
      }
      score1(snapshot, moves)
    }

    def score1(snapshot: GameSnapshot, moves: Move = null): Int = snapshot.getWinner match {
      case Player.Black => -1000
      case Player.White => 1000
      case Player.Draw => 0
      case Player.None => computeScore(snapshot)
    }

    def usefulValues(gameSnapshot: GameSnapshot): Unit = {
      val board = gameSnapshot.getBoard
      boardSize = board.size
      kingCoord = findKing(board.rows)
      kingOrthogonalCells = board.orthogonalCells(kingCoord)
      kingAdjacentCells = kingOrthogonalCells.map(_.take(1)).map(l => if(l.nonEmpty) Option(l.head) else Option.empty)
      cornerCoordinates = board.cornerCoordinates
      centralCoordinate = board.centerCoordinates
      quadrants = splitMatrixInFourPart(board.rows)
      blackCoord = board.rows.flatten.filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList
      gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
    }

    def computeScore(gameSnapshot: GameSnapshot): Int = {
      usefulValues(gameSnapshot)
      var score: Int = 0
      if(kingAdjacentToCorner())
        score += 900
      else {
        if(kingToCornerInOne()) {
          score += 900
        }
        if(kingCapturedInOne(gameSnapshot))
          score -= 900
        if(score == 0) {
          score -= computeBlackBetterPositions(gameSnapshot)
          score += computeWhiteBetterPositions(gameSnapshot)
        }
      }
      score
    }

    def kingAdjacentToCorner(): Boolean = kingAdjacentCells.filter(_.nonEmpty).map(_.get.getCoordinate).exists(cornerCoordinates.contains(_))

    // TODO VALUTARE ANCHE SE CATTURA SULL'ANGOLO
    def kingToCornerInOne(): Boolean = {
      var returnValue = false
      for(i <- kingOrthogonalCells.indices)
        if (kingOrthogonalCells(i).nonEmpty &&
          cornerCoordinates.contains(kingOrthogonalCells(i).last.getCoordinate) &&
          (kingOrthogonalCells(i).count(_.getPiece.equals(Piece.Empty)) == kingOrthogonalCells(i).size))
            returnValue = true
      returnValue
    }

    def kingCapturedInOne(gameSnapshot: GameSnapshot): Boolean = gameSnapshot.getVariant match {
      case GameVariant.Hnefatafl | GameVariant.Tawlbwrdd => kingCapturedInOneBigBoard()
      case GameVariant.Brandubh | GameVariant.Tablut => kingCapturedInOneSmallBoard(gameSnapshot)
    }

    def kingCapturedInOneBigBoard(): Boolean = {
      val emptyKingAdjacentCells = kingAdjacentCells.filter(_.nonEmpty).filter(_.get.getPiece.equals(Piece.Empty))
        .filter(!_.get.getCoordinate.equals(centralCoordinate))
      if (emptyKingAdjacentCells.size == 1) {
         gamePossibleMoves.map(_.to).contains(emptyKingAdjacentCells.head.get.getCoordinate)
      } else
        false
    }

    def kingCapturedInOneSmallBoard(gameSnapshot: GameSnapshot): Boolean = {
      var returnValue = false
      if (MoveGenerator.kingOnThrone(gameSnapshot, kingCoord) || MoveGenerator.kingNextToThrone(gameSnapshot, kingCoord))
        kingCapturedInOneBigBoard()
      else
        for(i <- kingAdjacentCells.indices)
          if(kingAdjacentCells(i).nonEmpty &&
            kingAdjacentCells(i).get.getPiece.equals(Piece.BlackPawn) &&
            kingAdjacentCells((i + 2) % 4).nonEmpty &&
            gamePossibleMoves.map(_.to).contains(kingAdjacentCells((i + 2) % 4).get.getCoordinate))
              returnValue = true
      returnValue
    }

    def computeWhiteBetterPositions(gameSnapshot: GameSnapshot): Int = {
      scoreKingNearCorners() +
      scoreKingIsInFreeRowOrColumn() +
      scoreKingMovesToAFreeCorner() +
      scoreCapturedBlack(gameSnapshot)
    }

    def computeBlackBetterPositions(gameSnapshot: GameSnapshot): Int = {
      //valutare se il re può andare in un angolo in una mossa successiva
      scoreBlackPawnProtectTheCorner() +
      scoreBlackSurroundTheKing() +
      scoreCapturedWhite(gameSnapshot)
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
      val rowWhithoutKing = kingOrthogonalCells(1) ++ kingOrthogonalCells.last
      val columnWhithoutKing = kingOrthogonalCells.head ++ kingOrthogonalCells(2)

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
      val score: Double = numberBlackPieceInKingQuadrant match {
        case 0 => 0
        case _ => 100 * 1 / numberBlackPieceInKingQuadrant
      }
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

    //Positive score if the black pieces protect the corners - black barricade
    def scoreBlackPawnProtectTheCorner(): Int = {
      def scoreBlackPawn(pawnCoord: Coordinate): Double = quadraticDistanceBetweenCells(findCloserCorner(pawnCoord), pawnCoord) match {
        case 1 => -200
        case distance =>  100 * 1 / distance
      }
      blackCoord.map(scoreBlackPawn).sum.toInt

    }

    //Positive score if the black pieces surround The King
    def scoreBlackSurroundTheKing(): Int = {
      def scoreBlackPawn(pawnCoord: Coordinate): Double = 100 * 1 / quadraticDistanceBetweenCells(pawnCoord, kingCoord)
      //aggiungere controllo se in celle adiacenti si può essere mangiati alla mossa successiva
      blackCoord.map(scoreBlackPawn).sum.toInt
    }

    //Positive score if the pawn is diagonal alignment with other pawns
    def scorePawnDiagonalAlignment(pawnCell: BoardCell, closeCell: Seq[BoardCell], score: Int = 0): Int = ???

    /*
    * Jolly rules
    */

    //Positive score if pieces (black or white) arranged in square
    def scorePawnArrangedInSquare(): Double = ???


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

      _isItDistantFromTheCornerOf(coord, distance, cornerCoordinates)
    }

    def transposeCoordinates(coord: Coordinate): Coordinate = coord match {
      case Coordinate(x, y) => Coordinate(y, x)
    }

    def whithoutKing(seq: Seq[BoardCell]): Seq[BoardCell] = seq.filter(cell => !cell.getPiece.equals(Piece.WhiteKing))

    //Quadratic distance
    def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate): Int = (scala.math.pow(start.x - end.x, 2) + scala.math.pow(start.y - end.y, 2)).toInt

    def findCloserCorner(coord: Coordinate): Coordinate = {
      def _getCloserCorner( closerCornerCord: Coordinate, closerDist: Int, cells: Seq[Coordinate] = cornerCoordinates ): Coordinate = cells match {
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

    def findKing(board: Seq[Seq[BoardCell]]): Coordinate =
      board.flatten.filter(_.getPiece.equals(Piece.WhiteKing)).map(_.getCoordinate).head
  }

}

object blabla extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Hnefatafl.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Hnefatafl, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)
  val ef: EvaluationFunction = EvaluationFunctionImpl()

  val newGs = MoveGenerator.makeMove(gameSnapshot, Move(Coordinate(6,6), Coordinate(4,3)))

  println(ef.score(newGs))



}