package ia

import actor_ia.{MoveGenerator, ScoreProvider}
import model._
import utils.BoardGame.OrthogonalDirection.OrthogonalDirection
import utils.BoardGame.{Board, BoardCell, OrthogonalDirection}
import utils.{Coordinate, Move}

import scala.collection.immutable.HashMap

object EvaluationFunction {

  private var boardSize: Int = _
  private var kingCoord: Coordinate = _
  private var cornerCoordinates: List[Coordinate] = _
  private var centralCoordinate: Coordinate = _
  private var quadrants: Seq[Seq[Seq[BoardCell]]] = _
  private var kingOrthogonalCells: Map[OrthogonalDirection, List[BoardCell]] = _
  private var kingAdjacentCells: Map[OrthogonalDirection, Option[BoardCell]] = _
  private var blackCoords: Seq[Coordinate] = _
  private var whiteCoords: Seq[Coordinate] = _
  private var gamePossibleMoves: Seq[Move] = _
  private var board: Board = _

  def score(snapshot: GameSnapshot): Int = snapshot.getWinner match {
    case Player.Black => -ScoreProvider.BlackWinScore
    case Player.White => ScoreProvider.WhiteWinScore
    case Player.Draw => ScoreProvider.DrawScore
    case _ => computeScore(snapshot)
  }

  def usefulValues(gameSnapshot: GameSnapshot): Unit = {
    board = gameSnapshot.getBoard
    boardSize = board.size
    kingCoord = MoveGenerator.findKing(board)

    kingOrthogonalCells = board.orthogonalCells(kingCoord)

    kingAdjacentCells = kingOrthogonalCells.map { case (k, v) => (k, v.take(1)) }
      .map { case (k, v) => if (v.nonEmpty) (k, Option(v.head)) else (k, Option.empty) }

    cornerCoordinates = board.cornerCoordinates
    centralCoordinate = board.centerCoordinates
    //quadrants = splitMatrixInFourPart(board.rows)
    blackCoords = board.rows.flatten.filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList
    whiteCoords = board.rows.flatten.filter(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing))
      .map(_.getCoordinate).toList
    gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
  }

  def computeScore(gameSnapshot: GameSnapshot): Int = {
    usefulValues(gameSnapshot)
    var score: Int = 0
    if (kingAdjacentToCorner()) {
      score += ScoreProvider.KingNearCornerScore
      println("PossibleKingAdjacentToCorner")
    }
    else if (kingToCornerInOne()) {
      score += ScoreProvider.KingToCornerScore
      println("PossibleKingToCornerInOne")
    }
    if (kingCapturedInOne(gameSnapshot)) {
      score -= ScoreProvider.KingCatchableInOne
      println("PossibleKingCaptureInOne")
    }
    if (score == 0) {
      val scoreRowOrColumnFree = scoreOwnerFirstLastThreeRowsOrColumns()
      println("RowsColumnsOwned(Whites,Blacks): " + scoreRowOrColumnFree)
      score -= computeBlackBetterPositions(gameSnapshot) + scoreRowOrColumnFree._2
      score += computeWhiteBetterPositions(gameSnapshot) + scoreRowOrColumnFree._1
    }
    score
  }

  def kingAdjacentToCorner(): Boolean = kingAdjacentCells.values.filter(_.nonEmpty).map(_.get.getCoordinate).exists(cornerCoordinates.contains(_))

  def kingToCornerInOne(): Boolean = {
    var returnValue = false
    for (i <- kingOrthogonalCells.keys)
      if (kingOrthogonalCells(i).nonEmpty &&
        cornerCoordinates.contains(kingOrthogonalCells(i).last.getCoordinate) &&
        (kingOrthogonalCells(i).count(_.getPiece.equals(Piece.Empty)) == kingOrthogonalCells(i).size))
        returnValue = true
    returnValue
  }

  def kingCapturedInOne(gameSnapshot: GameSnapshot): Boolean = gameSnapshot.getVariant match {
    case GameVariant.Hnefatafl | GameVariant.Tawlbwrdd => kingCapturedInOneBigBoard()
    case _ => kingCapturedInOneSmallBoard(gameSnapshot)
  }

  def kingCapturedInOneBigBoard(): Boolean = {
    val nonWhiteKingAdjacentCells = kingAdjacentCells.values.filter(_.nonEmpty).filter(!_.get.getPiece.equals(Piece.WhitePawn))
    if (nonWhiteKingAdjacentCells.size == 4) {
      val emptyKingAdjacentCells = nonWhiteKingAdjacentCells.filter(coord => coord.get.getPiece.equals(Piece.Empty) &&
        !coord.get.getCoordinate.equals(centralCoordinate))
      if (emptyKingAdjacentCells.size == 1) {
        gamePossibleMoves.map(_.to).contains(emptyKingAdjacentCells.head.get.getCoordinate)
      } else
        false
    } else
        false
  }

  def kingCapturedInOneSmallBoard(gameSnapshot: GameSnapshot): Boolean = {
    var returnValue = false
    if (MoveGenerator.kingOnThrone(gameSnapshot, kingCoord) || MoveGenerator.kingNextToThrone(gameSnapshot, kingCoord)) {
      returnValue = kingCapturedInOneBigBoard()
    }
    else
      for (i <- kingAdjacentCells.keys)
        if (kingAdjacentCells(i).nonEmpty &&
          kingAdjacentCells(i).get.getPiece.equals(Piece.BlackPawn) &&
          kingAdjacentCells(oppositeOrthogonalDirection(i)).nonEmpty &&
          gamePossibleMoves.map(_.to).contains(kingAdjacentCells(oppositeOrthogonalDirection(i)).get.getCoordinate))
          returnValue = true
    returnValue
  }

  private def oppositeOrthogonalDirection(orthogonalDirection: OrthogonalDirection): OrthogonalDirection = orthogonalDirection match {
    case OrthogonalDirection.Up => OrthogonalDirection.Down
    case OrthogonalDirection.Right => OrthogonalDirection.Left
    case OrthogonalDirection.Down => OrthogonalDirection.Up
    case OrthogonalDirection.Left => OrthogonalDirection.Right
  }

  def scoreKingOnThrone(/*gameSnapshot: GameSnapshot*/): Int = {
    if (kingCoord.equals(board.centerCoordinates)) ScoreProvider.KingOnThroneScore
    else if (quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord)) == 0) ScoreProvider.KingDistanceToCornerDividend
    else ScoreProvider.KingDistanceToCornerDividend / quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord))
  }

  def scoreTower(/*gameSnapshot: GameSnapshot*/): Int = {
    //val board = gameSnapshot.getBoard

    def isSquare(list: List[BoardCell]): Boolean = {
      list.count(cell => cell.getPiece.equals(Piece.WhitePawn) || cell.getPiece.equals(Piece.WhiteKing)) == 3
    }

    var score: Double = 0

    for {
      coord <- whiteCoords
      i = coord.x
      j = coord.y
      if i < boardSize && j < boardSize
      if isSquare(List(board.getCell(Coordinate(i, j + 1)),
        board.getCell(Coordinate(i + 1, j)),
        board.getCell(Coordinate(i + 1, j + 1))))
    } yield score += ScoreProvider.TowerCoefficient * quadraticDistanceBetweenCells(coord, centralCoordinate)

    score.toInt
  }


  def computeWhiteBetterPositions(gameSnapshot: GameSnapshot): Int = {

    println("***********")
    println("White King In Throne: " + scoreKingOnThrone(/*gameSnapshot*/))
    println("White Tower: " + scoreTower(/*gameSnapshot*/))
    println("King in Free Row or Column: " + scoreKingIsInFreeRowOrColumn())
    println("White Captured Black: " + scoreCapturedBlack(gameSnapshot))
    println("***********")

    scoreKingOnThrone(/*gameSnapshot*/) +
      scoreTower(/*gameSnapshot*/) +
      scoreKingIsInFreeRowOrColumn() +
      scoreCapturedBlack(gameSnapshot)
  }

  def computeBlackBetterPositions(gameSnapshot: GameSnapshot): Int = {

    println("***********")
    println("Black Surround The King: " + scoreBlackSurroundTheKing())
    println("Black Captured White: " + scoreCapturedWhite(gameSnapshot))
    println("Black Cordon: " + scoreBlackCordon())
    println("***********")

    scoreBlackSurroundTheKing() +
      scoreCapturedWhite(gameSnapshot) +
      //TODO review Cordon-Barricade Functionality and Score
      +scoreBlackCordon()

  }

  /**
   * White score
   */

  def scoreCapturedBlack(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedBlacks * ScoreProvider.BlackCapturedScore

  // Positive score if king is in free row or column

  def scoreKingIsInFreeRowOrColumn(): Int = {
    val rowWhithoutKing = kingOrthogonalCells(OrthogonalDirection.Right) ++ kingOrthogonalCells(OrthogonalDirection.Left)
    val columnWhithoutKing = kingOrthogonalCells(OrthogonalDirection.Up) ++ kingOrthogonalCells(OrthogonalDirection.Down)

    def _scoreKingIsInFreeRowOrColumn(): Int = (rowWhithoutKing, columnWhithoutKing) match {
      case (row, column) if isSequenceFreeCells(row) && isSequenceFreeCells(column) => ScoreProvider.KingRowAndColumnOwnerScore
      case (row, _) if isSequenceFreeCells(row) => ScoreProvider.KingRowOrColumnOwnerScore
      case (_, column) if isSequenceFreeCells(column) => ScoreProvider.KingRowOrColumnOwnerScore
      case _ => 0
    }

    _scoreKingIsInFreeRowOrColumn()
  }

  // Positive score if king moves to a free corner.
  // The score is inversely proportional to the concentration of black pawns in the king's quadrant.
  /*
  def scoreKingMovesToAFreeCorner(): Int = {
    val quadranKing = findQuadrant(kingCoord)
    val numberBlackPieceInKingQuadrant = quadranKing.flatten.count(cell => cell.getPiece.equals(Piece.BlackPawn))
    val score: Double = numberBlackPieceInKingQuadrant match {
      case 0 => 0
      case _ => 100 * 1 / numberBlackPieceInKingQuadrant
    }
    score.toInt
  }
  */
  /*
    // Positive score if the white opening in opposite side of black Blockade
    def scoreWhiteOpeningOnOppositeSideOfBlackBlockade(whiteCoord: Coordinate): Int = {
      val oppositeQuadrant: Seq[Seq[BoardCell]] = findQuadrant(whiteCoord: Coordinate, oppositQuadrant = true)
      val numberBlackPieceInOppositeKingQuadrant: Int = oppositeQuadrant.flatten.count(cell => cell.getPiece.equals(Piece.BlackPawn))
      val score: Int = numberBlackPieceInOppositeKingQuadrant * 2
      score
    }
  */


  // TODO ERA SOTTO


  /*
  * Jolly rules
  */

  def scoreOwnerFirstLastThreeRowsOrColumns(): (Int, Int) = {
    val boardTranspose = board.rows.toList.transpose
    val listRowsAndColumns: Seq[Seq[BoardCell]] = board.rows.take(3).toList ::: board.rows.takeRight(3).toList ::: boardTranspose.take(3) ::: boardTranspose.takeRight(3)
    var whiteScore = 0
    var blackScore = 0

    listRowsAndColumns.foreach(
      isRowOrColumnOwner(_) match {
        case Piece.WhiteKing => whiteScore += ScoreProvider.KingFirstLastRowOrColumnOwnerScore
        case Piece.WhitePawn => whiteScore += ScoreProvider.WhiteFirstLastRowOrColumnOwnerScore
        case Piece.BlackPawn => blackScore += ScoreProvider.BlackFirstLastRowOrColumnOwnerScore
        case _ =>
      }
    )
    (whiteScore, blackScore)
  }

  def wrongBarricadeScore(): (Int, Int) = {
    var whiteScore = 0
    var blackScore = 0
    board.cornerCoordinates.flatMap(c => getOrthogonalCoords(c)).filter(p => p._2 != Option.empty).grouped(2).foreach {
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get)).equals(Piece.BlackPawn) => blackScore += -ScoreProvider.WrongBarricade
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get)).equals(Piece.WhitePawn) => whiteScore += -ScoreProvider.WrongBarricade
      case _ =>
    }
    (whiteScore, blackScore)
  }

  def wrongBarricadeType(coord1: BoardCell, coord2: BoardCell): Piece.Value = (coord1.getPiece, coord2.getPiece) match {
    case (Piece.BlackPawn, Piece.BlackPawn) => Piece.BlackPawn
    case (Piece.WhitePawn, Piece.WhitePawn) => Piece.WhitePawn
    case _ => Piece.Empty
  }


  /* UTILS METHODS */


  def isRowOrColumnOwner(cellsSeq: Seq[BoardCell]): Piece.Value = {
    val nonEmptyCells = cellsSeq.filter(!_.getPiece.equals(Piece.Empty))
    if ((nonEmptyCells.size == 1) && nonEmptyCells.head.getPiece.equals(Piece.WhiteKing))
      Piece.WhiteKing
    else if (samePlayerPawns(nonEmptyCells, Piece.WhitePawn))
      Piece.WhitePawn
    else if (samePlayerPawns(nonEmptyCells, Piece.BlackPawn))
      Piece.BlackPawn
    else
      Piece.Empty
  }

  private def samePlayerPawns(l: Seq[BoardCell], p: Piece.Val): Boolean =
    l.nonEmpty && l.size.equals(l.count(_.getPiece.equals(p)))

  /*
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
  */

  def isItDistantFromCornerOf(coord: Coordinate, distance: Int): Boolean = {

    @scala.annotation.tailrec
    def _isItDistantFromTheCornerOf(coord: Coordinate, distance: Int, cornerCoord: List[Coordinate]): Boolean = cornerCoord match {
      case Nil => false
      case h :: _ if quadraticDistanceBetweenCells(h, coord) == distance => true
      case _ :: t => _isItDistantFromTheCornerOf(coord, distance, t)
    }

    _isItDistantFromTheCornerOf(coord, distance, cornerCoordinates)
  }

  /*
    def transposeCoordinates(coord: Coordinate): Coordinate = coord match {
      case Coordinate(x, y) => Coordinate(y, x)
    }

    def withoutKing(seq: Seq[BoardCell]): Seq[BoardCell] = seq.filter(cell => !cell.getPiece.equals(Piece.WhiteKing))
  */
  //Quadratic distance
  def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate): Int = (scala.math.pow(start.x - end.x, 2) + scala.math.pow(start.y - end.y, 2)).toInt

  def findCloserCorner(coord: Coordinate): Coordinate = {
    def _getCloserCorner(closerCornerCord: Coordinate, closerDist: Int, cells: Seq[Coordinate] = cornerCoordinates): Coordinate = cells match {
      case Nil => closerCornerCord
      case h :: t =>
        val distCoordFromCorner = quadraticDistanceBetweenCells(h, coord)
        val (newCloserCorner, newCloserDist) = if (distCoordFromCorner > closerDist) (closerCornerCord, closerDist) else (h, distCoordFromCorner)
        _getCloserCorner(newCloserCorner, newCloserDist, t)
    }

    _getCloserCorner(Coordinate(1, 1), Int.MaxValue)
  }

  /*
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
*/
  def isSequenceFreeCells(seq: Seq[BoardCell]): Boolean =
    seq.count(boardCell => !boardCell.getPiece.equals(Piece.Empty)) == 0


  // TODO QUI PARTIVA


  /**
   * Black score
   */
  def scoreCapturedWhite(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedWhites * ScoreProvider.WhiteCapturedScore

  //Positive score if the black pieces surround The King
  def scoreBlackSurroundTheKing(): Int = {
    kingAdjacentCells.values.count(c => c.nonEmpty && c.get.getPiece.equals(Piece.BlackPawn)) * ScoreProvider.BlackNearKing
  }

  def scoreBlackOnKingDiagonal(): Int = {
    var score = 0
    kingCoord = MoveGenerator.findKing(board)
    findNearBlacks(kingCoord).filter(c => (c.x != kingCoord.x) && (c.y != kingCoord.y)).foreach(c => board.getCell(c).getPiece match {
      case Piece.BlackPawn => score += 25
      case _ =>
    })
    score
  }

  def scoreBlackCordon(): Int = {
    var cordons: Seq[Seq[Coordinate]] = Seq.empty

    blackCoords.foreach(b => {
      if (!cordons.flatten.contains(b))
        cordons :+= createCordon(b, Seq.empty)
    })
    val listCordons = cordons.distinct.filter(_.size >= 3)
    var score = 0

    listCordons.foreach({score += isCorrectCordon(_)})

    score
    //listCordons.flatten.size * ScoreProvider.CordonPawn + score
  }

  def createCordon(fromCoordinate: Coordinate, cordon: Seq[Coordinate]): Seq[Coordinate] = {

    def _createCordon(fromCoordinate: Coordinate, cordon: Seq[Coordinate]): Seq[Coordinate] = {
      @annotation.tailrec
      def _explore(coordinates: Seq[Coordinate], cordon: Seq[Coordinate]): Seq[Coordinate] =
        coordinates match {
          case Seq() => cordon
          case _ =>
            val nears = coordinates.flatMap(c => findNearBlacks(c).filter(!cordon.contains(_)))
            _explore(nears, (cordon ++ nears).distinct)
      }
      _explore(Seq(fromCoordinate), cordon)
    }

    val result = _createCordon(fromCoordinate, cordon)
    result.filter(c => !isRedundant(c, result)).sorted

  }

  def isRedundant(coordinate: Coordinate, cordon: Seq[Coordinate]): Boolean = {
    val cycles = get3CycleCoordinates(coordinate)
    for (d <- OrthogonalDirection.values) {
      if (cycles(d).size == 2 && isSubList(cycles(d), cordon))
        return true
    }
    false
  }

  def isSubList(subList: Seq[Any], list: Seq[Any]): Boolean = subList.forall(list.contains)

  def get3CycleCoordinates(c: Coordinate): Map[OrthogonalDirection, List[Coordinate]] = {
    HashMap(
      OrthogonalDirection.Up ->
        List(getOrthogonalCoords(c)(OrthogonalDirection.Left),
          getOrthogonalCoords(c)(OrthogonalDirection.Up)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Right ->
        List(getOrthogonalCoords(c)(OrthogonalDirection.Up),
          getOrthogonalCoords(c)(OrthogonalDirection.Right)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Down ->
        List(getOrthogonalCoords(c)(OrthogonalDirection.Right),
          getOrthogonalCoords(c)(OrthogonalDirection.Down)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Left ->
        List(getOrthogonalCoords(c)(OrthogonalDirection.Down),
          getOrthogonalCoords(c)(OrthogonalDirection.Left)).filter(_.nonEmpty).map(_.get)
    )
  }

  def getOrthogonalCoords(coordinate: Coordinate): Map[OrthogonalDirection, Option[Coordinate]] =
    HashMap(
      OrthogonalDirection.Up ->
        filterOutOfBoardCoord(Coordinate(coordinate.x - 1, coordinate.y)),
      OrthogonalDirection.Right ->
        filterOutOfBoardCoord(Coordinate(coordinate.x, coordinate.y + 1)),
      OrthogonalDirection.Down ->
        filterOutOfBoardCoord(Coordinate(coordinate.x + 1, coordinate.y)),
      OrthogonalDirection.Left ->
        filterOutOfBoardCoord(Coordinate(coordinate.x, coordinate.y - 1))
    )

  def filterOutOfBoardCoord(coordinate: Coordinate): Option[Coordinate] =
    if (!outOfBoardCoord(coordinate))
      Option(coordinate)
    else
      Option.empty

  def outOfBoardCoord(coordinate: Coordinate): Boolean =
    coordinate.x > boardSize || coordinate.x < 1 || coordinate.y > boardSize || coordinate.y < 1


  /*def filterOutOfBoardCoords(list: List[Coordinate]): List[Coordinate] =
    list.filter(coord => coord.x <= boardSize && coord.x >= 1 && coord.y <= boardSize && coord.y >= 1)*/

  def findNearBlacks(c: Coordinate): Seq[Coordinate] = {

    List(Coordinate(c.x + 1, c.y), Coordinate(c.x, c.y + 1),
      Coordinate(c.x - 1, c.y), Coordinate(c.x, c.y - 1),
      Coordinate(c.x - 1, c.y - 1), Coordinate(c.x + 1, c.y + 1),
      Coordinate(c.x - 1, c.y + 1), Coordinate(c.x + 1, c.y - 1))
      .filter(coord => coord.x <= boardSize && coord.x >= 1 &&
        coord.y <= boardSize && coord.y >= 1)
      .map(board.getCell).filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate)
  }

  /*
  def getDiagonalBlackCoords(coordinate: Coordinate): List[Coordinate] =
    filterBlackCoordinates(getDiagonalCoords(coordinate))

  def getDiagonalCoords(coordinate: Coordinate): List[Coordinate] =
    filterOutOfBoardCoords(List(Coordinate(coordinate.x - 1, coordinate.y - 1 ), Coordinate(coordinate.x +1, coordinate.y + 1),
      Coordinate(coordinate.x - 1, coordinate.y + 1 ), Coordinate(coordinate.x +1, coordinate.y - 1)))

  def getOrthogonalBlackCoords(coordinate: Coordinate): List[Coordinate] =
    filterBlackCoordinates(getOrthogonalCoords(coordinate))



  def filterBlackCoordinates(list: List[Coordinate]): List[Coordinate] =
    list.map(board.getCell).filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate)

*/

  def getOtherSides(coordinateOnSide: Coordinate): Seq[OrthogonalDirection] = coordinateOnSide match {
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Up) =>
      Seq(OrthogonalDirection.Right, OrthogonalDirection.Down, OrthogonalDirection.Left)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Right) =>
      Seq(OrthogonalDirection.Up, OrthogonalDirection.Down, OrthogonalDirection.Left)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Down) =>
      Seq(OrthogonalDirection.Up, OrthogonalDirection.Right, OrthogonalDirection.Left)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Left) =>
      Seq(OrthogonalDirection.Up, OrthogonalDirection.Down, OrthogonalDirection.Right)
    case _ => Seq.empty
  }

  def isOnSide(coordinate: Coordinate, side: OrthogonalDirection): Boolean = side match {
    case OrthogonalDirection.Up => coordinate.x == 1
    case OrthogonalDirection.Right => coordinate.y == boardSize
    case OrthogonalDirection.Down => coordinate.x == boardSize
    case OrthogonalDirection.Left => coordinate.y == 1
    case _ => false
  }

  def isOnAnySides(coordinate: Coordinate, sides: Seq[OrthogonalDirection] = Seq(OrthogonalDirection.Up, OrthogonalDirection.Down, OrthogonalDirection.Right, OrthogonalDirection.Left)): Boolean =
    sides.exists(isOnSide(coordinate, _))


  /**
   * No black piece can sit on a corner so no double sides.
   */
  def getCurrentSide(coordinateOnSide: Coordinate): Option[OrthogonalDirection] = coordinateOnSide match {
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Up) => Option(OrthogonalDirection.Up)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Right) => Option(OrthogonalDirection.Right)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Down) => Option(OrthogonalDirection.Down)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Left) => Option(OrthogonalDirection.Left)
    case _ => Option.empty
  }

  def isCorrectCordon(cordon: Seq[Coordinate]): Int = {
    if(isCircleCordon(cordon))
      checkCircleCordon(cordon)
    else checkNotCircleCordon(cordon)
  }

  def checkCircleCordon(cordon: Seq[Coordinate]): Int = {
    val (_,inn) = splitBoardWithCircleCordon(cordon)
    if(isCorrectCircleCordon(inn))
      cordon.size * ScoreProvider.CordonPawn + ScoreProvider.RightCordon
    else
      cordon.size * ScoreProvider.CordonPawn - ScoreProvider.WrongCordon
  }

  def checkNotCircleCordon(cordon: Seq[Coordinate]): Int = {
    if(isCorrectNotCircleCordon(cordon)) {
      var sequences: (Seq[BoardCell],Seq[BoardCell]) = (Seq.empty,Seq.empty)

      if (isHorizontalCordon(cordon))
        sequences = splitBoardWithHorizontalCordon(cordon)
      else
        sequences = splitBoardWithVerticalCordon(cordon)
      println(controlEmptyPortion(sequences._1) )
      println(controlEmptyPortion(sequences._2))
      if (controlEmptyPortion(sequences._1) || controlEmptyPortion(sequences._2)){
        cordon.size * ScoreProvider.CordonPawn + ScoreProvider.RightBarricade
      }
      else
        0
    } else
      0
  }

  private def controlEmptyPortion(sequence: Seq[BoardCell]): Boolean =
    sequence.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) == 0

  def isCorrectCircleCordon(innerCells: Seq[BoardCell]): Boolean = innerCells.count(_.getPiece.equals(Piece.Empty)) == 0 ||
    innerCells.count(_.getPiece.equals(Piece.WhitePawn)) > 3 || innerCells.exists(_.getPiece.equals(Piece.WhiteKing))

  def isCorrectNotCircleCordon(cordon: Seq[Coordinate]): Boolean =
    cordon.filter(isOnAnySides(_)).map(getCurrentSide).filter(_.nonEmpty).map(_.get).distinct.size >= 2
    //cordon.count(getCurrentSide(_).get.equals(getCurrentSide(cordon.head).get)) == cordon.size

  /*
  def splitBoardWithCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {

    if(isCircleCordon(cordon)){
      splitBoardWithCircleCordon(cordon)
    }else if(isHorizontalCordon(cordon)){
      splitBoardWithNotCircleCordon(cordon, boardTranspose)
    }else{
      splitBoardWithNotCircleCordon(cordon, board)
    }

  }
  */

  def splitBoardWithCircleCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {

    @scala.annotation.tailrec
    def _splitBoardWithCircleCordon(cordon: Seq[Coordinate], output: Seq[(Seq[BoardCell],Seq[BoardCell])]):Seq[(Seq[BoardCell],Seq[BoardCell])] = cordon match {
      case Nil => output
      case h1::h2::t if  h1.x == h2.x  => _splitBoardWithCircleCordon(t, output :+ getRightAndLeft(h1,h2))
      case _::t => _splitBoardWithCircleCordon(t, output)

    }
    def getRightAndLeft( right: Coordinate, left: Coordinate): (Seq[BoardCell],Seq[BoardCell]) =
      (getSpecificOrthogonalCell(right, OrthogonalDirection.Right, board), getSpecificOrthogonalCell(left, OrthogonalDirection.Left, board))

    val inner = _splitBoardWithCircleCordon(cordon, Seq.empty).flatMap(elem => elem._1.intersect(elem._2)).toList

    val external = board.rows.flatten.diff(inner).filter(c => !cordon.contains(c.getCoordinate)).toList
    (external,inner)

  }

  def splitBoardWithHorizontalCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {
    splitBoardWithNotCircleCordon(cordon,OrthogonalDirection.Up)  }

  def splitBoardWithVerticalCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {
    splitBoardWithNotCircleCordon(cordon,OrthogonalDirection.Left)
  }

  def splitBoardWithNotCircleCordon(cordon: Seq[Coordinate], orthogonalDirection: OrthogonalDirection) : (Seq[BoardCell], Seq[BoardCell]) = {
    val leftCordonCells: Seq[BoardCell] = cordon.flatMap(getSpecificOrthogonalCell(_, orthogonalDirection, board)).toList
    val rightCordonCells: Seq[BoardCell] = board.rows.flatten.diff(leftCordonCells).filter(cell => !cordon.contains( cell.getCoordinate)).toList
    (leftCordonCells, rightCordonCells)
  }

  def getSpecificOrthogonalCell(coord: Coordinate, orthogonalDirection: OrthogonalDirection, localBoard: Board ):Seq[BoardCell] = {
    val map = localBoard.orthogonalCells(coord)
    if( map.keySet.contains(orthogonalDirection)){
      return map(orthogonalDirection)
    }
    Seq.empty
  }

  def isCircleCordon( cordon: Seq[Coordinate]): Boolean =
    cordon.count(findNearBlacks(_).count(cordon.contains(_)) >= 2) == cordon.size

  def isHorizontalCordon( cordon: Seq[Coordinate]): Boolean = //cordon.head.x == cordon.last.x
    cordon.filter(isOnAnySides(_))
      .map(getCurrentSide)
      .filter(_.nonEmpty)
      .map(_.get)
      .distinct
      .count(c => c.equals(OrthogonalDirection.Left)  || c.equals(OrthogonalDirection.Right)) >= 2
}

object blabla extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val prolog: ParserProlog = ParserPrologImpl(THEORY)

  /*
  val game = prolog.createGame(GameVariant.Tawlbwrdd.nameVariant.toLowerCase)
  val snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game_3, Option.empty, 0, 0)
  EvaluationFunction.usefulValues(snapshot)
  println(EvaluationFunction.getOtherSides(Coordinate(6,6)))
  println(EvaluationFunction.isOnSide(Coordinate(11,1), OrthogonalDirection.Down))
  println(EvaluationFunction.isOnAnySides(Coordinate(11,2), List(OrthogonalDirection.Up, OrthogonalDirection.Right, OrthogonalDirection.Left)))
  println(EvaluationFunction.getCurrentSide(Coordinate(6,6)))

  var l: ListBuffer[Int] = ListBuffer(1, 2, 3, 4)
  val l1: ListBuffer[Int] = ListBuffer(2, 3,1 ,5, 4)
  val l2: ListBuffer[Int] = (l ++ l1).distinct
  println(EvaluationFunction.isSubList(l, l1))*/


  var game = prolog.createGame(GameVariant.Tawlbwrdd.nameVariant.toLowerCase)
  var snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)
  EvaluationFunction.usefulValues(snapshot)
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(1,2)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,1)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,7), Coordinate(1,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,7), Coordinate(2,11)))

  println(EvaluationFunction.wrongBarricadeScore())

}