package ia


import actor_ia.MoveGenerator

import model._
import utils.BoardGame.{Board, BoardCell}
import utils.{Coordinate, Move}

object EvaluationFunction {

  private var boardSize: Int = _
  private var kingCoord: Coordinate = _
  private var cornerCoordinates: List[Coordinate] = _
  private var centralCoordinate: Coordinate = _
  private var quadrants: Seq[Seq[Seq[BoardCell]]]= _
  private var kingOrthogonalCells: Seq[Seq[BoardCell]] = _
  private var kingAdjacentCells: Seq[Option[BoardCell]] = _
  private var blackCoords: Seq[Coordinate] = _
  private var whiteCoords: Seq[Coordinate] = _
  private var gamePossibleMoves: Seq[Move] = _
  private var board: Board = _

  def score(snapshot: GameSnapshot): Int = snapshot.getWinner match {
    case Player.Black => -1000
    case Player.White => 1000
    case Player.Draw => 0
    case Player.None => computeScore(snapshot)
  }

  def usefulValues(gameSnapshot: GameSnapshot): Unit = {
    board = gameSnapshot.getBoard
    boardSize = board.size
    kingCoord = MoveGenerator.findKing(board)
    //TODO utilizzare mappa(OrthogonalDirection, List) (togliere .values.toList)
    kingOrthogonalCells = board.orthogonalCells(kingCoord).values.toList
    kingAdjacentCells = kingOrthogonalCells.map(_.take(1)).map(l => if(l.nonEmpty) Option(l.head) else Option.empty)
    cornerCoordinates = board.cornerCoordinates
    centralCoordinate = board.centerCoordinates
    quadrants = splitMatrixInFourPart(board.rows)
    blackCoords = board.rows.flatten.filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList
    whiteCoords = board.rows.flatten.filter(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing))
      .map(_.getCoordinate).toList
    gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
  }

  def computeScore(gameSnapshot: GameSnapshot): Int = {
    usefulValues(gameSnapshot)
    var score: Int = 0
    if(kingAdjacentToCorner())
      score += 900
    else
      if(kingToCornerInOne())
        score += 900
      if(kingCapturedInOne(gameSnapshot))
        score -= 900
      if(score == 0) {
        val scoreRowOrColumnFree = scoreOwnerFirstLastThreeRowsOrColumns()
        score -= computeBlackBetterPositions(gameSnapshot) + scoreRowOrColumnFree._2
        score += computeWhiteBetterPositions(gameSnapshot) + scoreRowOrColumnFree._1
    }
    score
  }

  def kingAdjacentToCorner(): Boolean = kingAdjacentCells.filter(_.nonEmpty).map(_.get.getCoordinate).exists(cornerCoordinates.contains(_))

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
    if (MoveGenerator.kingOnThrone(gameSnapshot, kingCoord) || MoveGenerator.kingNextToThrone(gameSnapshot, kingCoord)) {
      returnValue = kingCapturedInOneBigBoard()
    }
    else
      for(i <- kingAdjacentCells.indices)
        if(kingAdjacentCells(i).nonEmpty &&
          kingAdjacentCells(i).get.getPiece.equals(Piece.BlackPawn) &&
          kingAdjacentCells((i + 2) % 4).nonEmpty &&
          gamePossibleMoves.map(_.to).contains(kingAdjacentCells((i + 2) % 4).get.getCoordinate))
            returnValue = true
    returnValue
  }

  def scoreKingOnThrone(gameSnapshot: GameSnapshot): Int = {
    if(kingCoord.equals(gameSnapshot.getBoard.centerCoordinates)) 0
    else if (quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord)) == 0) 150
    else 150 / quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord))
  }

  def scoreTower(gameSnapshot: GameSnapshot): Int = {
    val board = gameSnapshot.getBoard

    def isSquare(list: List[BoardCell]):Boolean = {
      list.count(cell => cell.getPiece.equals(Piece.WhitePawn) || cell.getPiece.equals(Piece.WhiteKing)) == 3
    }

    var score: Double = 0

    for {
      coord <- whiteCoords
      i = coord.x
      j = coord.y
      if i < boardSize && j < boardSize
      if isSquare(List(board.getCell(Coordinate(i, j+1)),
        board.getCell(Coordinate(i+1, j)),
        board.getCell(Coordinate(i+1, j+1))))
    } yield score += 1.25 * quadraticDistanceBetweenCells(coord, centralCoordinate)

    score.toInt
  }


  def computeWhiteBetterPositions(gameSnapshot: GameSnapshot): Int = {

    /*
    println("***********")
    println("White King In Throne: " + scoreKingInThrone(gameSnapshot))
    println("White Tower: " + scoreTower(gameSnapshot))
    println("King in Free Row or Column: " + scoreKingIsInFreeRowOrColumn())
    println("White Captured Black: " + scoreCapturedBlack(gameSnapshot))
    println("***********")
    println()
    */
    scoreKingOnThrone(gameSnapshot) +
    scoreTower(gameSnapshot) +
    scoreKingIsInFreeRowOrColumn() +
    scoreCapturedBlack(gameSnapshot)
  }

  def computeBlackBetterPositions(gameSnapshot: GameSnapshot): Int = {
    //valutare se il re può andare in un angolo in una mossa successiva

    /*
    println("***********")
    println("Black Protect Corner: " + scoreBlackPawnProtectTheCorner())
    println("Black Surround The King: " + scoreBlackSurroundTheKing())
    println("Black Captured White: " + scoreCapturedWhite(gameSnapshot))
    println("***********")
    */

    //TODO scoreBlackPawnProtectTheCorner() +
    scoreBlackSurroundTheKing() +
    scoreCapturedWhite(gameSnapshot)
    //TODO scoreCordon() +

  }

  /* RULES */

  /**
  * White score
  */

  def scoreCapturedBlack(gameSnapshot: GameSnapshot):Int = gameSnapshot.getNumberCapturedBlacks * 10

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
    val oppositeQuadrant: Seq[Seq[BoardCell]] = findQuadrant(whiteCoord: Coordinate, oppositQuadrant = true)
    val numberBlackPieceInOppositeKingQuadrant: Int = oppositeQuadrant.flatten.count(cell => cell.getPiece.equals(Piece.BlackPawn))
    val score: Int = numberBlackPieceInOppositeKingQuadrant * 2
    score
  }

  /**
  * Black score
  */
  def scoreCapturedWhite(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedWhites * 15

  //Positive score if the black pieces protect the corners - black barricade
  def scoreBlackPawnProtectTheCorner(): Int = {
    def scoreBlackPawn(pawnCoord: Coordinate): Double = quadraticDistanceBetweenCells(findCloserCorner(pawnCoord), pawnCoord) match {
      case 1 => -20
      case distance =>  100 / distance
    }
    blackCoords.map(scoreBlackPawn).sum.toInt

  }

  //Positive score if the black pieces surround The King
  def scoreBlackSurroundTheKing(): Int = {
    /*
    def scoreBlackPawn(pawnCoord: Coordinate): Double = 100 / quadraticDistanceBetweenCells(pawnCoord, kingCoord)
    //aggiungere controllo se in celle adiacenti si può essere mangiati alla mossa successiva
    blackCoords.map(scoreBlackPawn).sum.toInt
    */
    kingAdjacentCells.count(c => c.nonEmpty && c.get.getPiece.equals(Piece.BlackPawn)) * 30
  }

  def scoreBlackCordon():Int = {

    def getOtherSide(coordinate: Coordinate):List[Coordinate] = coordinate match {
      case Coordinate(x,_) if x == boardSize => List(Coordinate(-1,x), Coordinate(1,-1), Coordinate(-1,1))
      case Coordinate(_,x) if x == boardSize => List(Coordinate(x,-1), Coordinate(1,-1), Coordinate(-1,1))
      case Coordinate(1,_) => List(Coordinate(-1,1) , Coordinate(boardSize,-1), Coordinate(-1,boardSize))
      case _ => List(Coordinate(1,-1) , Coordinate(boardSize,-1), Coordinate(-1,boardSize))
    }

    def isOnAnySides(currentCoord:Coordinate, otherSideCoord: List[Coordinate]):Boolean = otherSideCoord.exists(isOnSide(currentCoord, _))

    def isOnSide(coord:Coordinate, startSideCoord: Coordinate ): Boolean = coord.x == startSideCoord.x || coord.y == startSideCoord.y

    def findNearBlack(c: Coordinate):List[Coordinate] = {
      val nearCord = List(Coordinate(c.x + 1, c.y),Coordinate(c.x, c.y + 1),
        Coordinate(c.x -1 , c.y),Coordinate(c.x, c.y - 1),
        Coordinate(c.x - 1, c.y - 1 ),Coordinate(c.x +1, c.y + 1),
        Coordinate(c.x - 1, c.y + 1 ),Coordinate(c.x +1, c.y - 1))
        .filter(coord => coord.x <= boardSize && coord.x >= 1 &&
          coord.y <= boardSize && coord.y >= 1)

      nearCord.map(board.getCell).filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate)
    }

    def countPawnInCordon(previousCoord:List[Coordinate], nearPawnsCoords:List[Coordinate], otherSideCoord: List[Coordinate], startSideCoord: Coordinate, count:Int ):Int = nearPawnsCoords match {
      case Nil => count //false
      case h::_ if isOnAnySides(h, otherSideCoord) => count + 1 //true
      case h::t if previousCoord.contains(h) || isOnSide(h, startSideCoord )  => countPawnInCordon(previousCoord, t, otherSideCoord,startSideCoord, count)
      case h::t => countPawnInCordon( previousCoord, t, otherSideCoord,startSideCoord, count + countPawnInCordon(previousCoord :+ h, findNearBlack(h), otherSideCoord, startSideCoord, count ))
    }
    //ToDo tovare un modo per iterare sui quattro lati
    val prova = board.rows.head
      .filter(_.getPiece.equals(Piece.BlackPawn))
      .map(_.getCoordinate)
      .map( x => countPawnInCordon( List(x), findNearBlack(x), getOtherSide(x),Coordinate(1,-1), 1)).filter(_>=3)
    println(prova)
    prova.sum

  }



  /*
  * Jolly rules
  */

  def scoreOwnerFirstLastThreeRowsOrColumns(): (Int, Int) = {
    val boardTranspose =  board.rows.toList.transpose
    val listRowsAndColumns: Seq[Seq[BoardCell]] = board.rows.take(3).toList ::: board.rows.takeRight(3).toList ::: boardTranspose.take(3) ::: boardTranspose.takeRight(3)
    var whiteScore = 0
    var blackScore = 0

   listRowsAndColumns.foreach(
      isRowOrColumnOwner(_) match {
        case Piece.WhiteKing => whiteScore += 90
        case Piece.WhitePawn => whiteScore += 10
        case Piece.BlackPawn => blackScore += 10
        case _ =>
      }
   )
    (whiteScore, blackScore)
  }


  /* UTILS METHODS */


  def isRowOrColumnOwner(cellsSeq: Seq[BoardCell]): Piece.Value = {
    val nonEmptyCells = cellsSeq.filter(!_.getPiece.equals(Piece.Empty))
    if((nonEmptyCells.size == 1) && nonEmptyCells.head.getPiece.equals(Piece.WhiteKing))
      Piece.WhiteKing
    else if(samePlayerPawns(nonEmptyCells, Piece.WhitePawn))
      Piece.WhitePawn
    else if(samePlayerPawns(nonEmptyCells, Piece.BlackPawn))
      Piece.BlackPawn
    else
      Piece.Empty
  }

  private def samePlayerPawns(l: Seq[BoardCell], p: Piece.Val): Boolean =
    l.nonEmpty && l.size.equals(l.count(_.getPiece.equals(p)))

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

    @scala.annotation.tailrec
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

  def withoutKing(seq: Seq[BoardCell]): Seq[BoardCell] = seq.filter(cell => !cell.getPiece.equals(Piece.WhiteKing))

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
}

object blabla extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Hnefatafl.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Hnefatafl, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)

  val newGs = MoveGenerator.makeMove(gameSnapshot, Move(Coordinate(6,6), Coordinate(4,3)))

  println(EvaluationFunction.score(newGs))
}