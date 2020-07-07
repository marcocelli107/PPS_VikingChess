package ia

import actor_ia.{ArtificialIntelligenceImpl, FindBestMoveMsg, MoveGenerator, ScoreProvider}
import akka.actor.{ActorRef, ActorSystem, Props}
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
  private var kingOrthogonalCells: Map[OrthogonalDirection, List[BoardCell]] = _
  private var kingAdjacentCells: Map[OrthogonalDirection, Option[BoardCell]] = _
  private var blackCoords: Seq[Coordinate] = _
  private var whiteCoords: Seq[Coordinate] = _
  private var gamePossibleMoves: Seq[Move] = _
  private var board: Board = _


  def score(snapshot: GameSnapshot, levelIA: Level.Val): Int = snapshot.getWinner match {
    case Player.Black => -ScoreProvider.BlackWin
    case Player.White => ScoreProvider.WhiteWin
    case Player.Draw => ScoreProvider.Draw
    case _ => computeScore(snapshot, levelIA)
  }

  def usefulValues(gameSnapshot: GameSnapshot): Unit = {
    board = gameSnapshot.getBoard
    boardSize = board.size

    kingCoord = MoveGenerator.findKing(board)
    kingOrthogonalCells = board.orthogonalCells(kingCoord)
    kingAdjacentCells = mappingOrthogonalToAdjacent(kingOrthogonalCells)

    cornerCoordinates = board.cornerCoordinates
    centralCoordinate = board.centerCoordinates

    blackCoords = board.rows.flatten.filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList
    whiteCoords = board.rows.flatten.filter(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing))
      .map(_.getCoordinate).toList

    gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
  }

  def computeScore(gameSnapshot: GameSnapshot, levelIA: Level.Val): Int = {
    usefulValues(gameSnapshot)
    var score: Int = 0

    levelIA match {
      case Level.Newcomer => score = computeNewcomerScore(gameSnapshot)
      case Level.Standard => score = computeStandardScore(gameSnapshot)
      case _ => score = computeAdvancedScore(gameSnapshot)
    }

    score
    /*
    if (kingAdjacentToCorner()) {
      score += ScoreProvider.KingNearCorner
      //println("PossibleKingAdjacentToCorner")
    }
    else if (kingToCornerInOne()) {
      score += ScoreProvider.KingToCorner
      //println("PossibleKingToCornerInOne")
    }
    if (kingCapturedInOne(gameSnapshot)) {
      score -= ScoreProvider.KingCatchableInOne
      //println("PossibleKingCaptureInOne")
    }
    if (score == 0) {
      val scoreRowOrColumnFree: (Int, Int) = scoreOwnerFirstLastThreeRowsOrColumns()
      val scoreErrouneousBarricade: (Int, Int) = scoreWrongBarricade()
      //println("RowsColumnsOwned(Whites,Blacks): " + scoreRowOrColumnFree)
      //println("WrongBarricade(Whites, Blacks)" + scoreWrongBarricade)
      score -= computeBlackBetterPositions(gameSnapshot) + scoreRowOrColumnFree._2 - scoreErrouneousBarricade._2
      score += computeWhiteBetterPositions(gameSnapshot) + scoreRowOrColumnFree._1 - scoreErrouneousBarricade._1
    }
    score*/
  }

  def computeNewcomerScore(snapshot: GameSnapshot): Int = {
    var newcomerScore: Int = 0

    if (kingAdjacentToCorner()) {
      newcomerScore += ScoreProvider.KingNearCorner
      //println("PossibleKingAdjacentToCorner")
    }
    else if (kingToCornerInOne()) {
      newcomerScore += ScoreProvider.KingToCorner
      //println("PossibleKingToCornerInOne")
    }
    if(newcomerScore == 0) {
      //println(scoreBlackSurroundTheKing() + scoreBlackOnKingDiagonal() + scoreCapturedWhite(snapshot))
      //(scoreCapturedBlack(snapshot) + scoreKingOnThrone() +
        //scoreKingIsInFreeRowOrColumn())

      newcomerScore -= scoreBlackSurroundTheKing() +
                        scoreBlackOnKingDiagonal() +
                        scoreCapturedWhite(snapshot) 
      newcomerScore += scoreKingOnThrone() +
                        scoreKingIsInFreeRowOrColumn() +
                        scoreCapturedBlack(snapshot)
    }
    newcomerScore
  }

  //TODO togliere print inutili dopo aver verificato punteggi corretti
  def computeStandardScore(snapshot: GameSnapshot): Int = {
    var standardScore: Int = computeNewcomerScore(snapshot)
    //println("INNER NEWCOMER: " + standardScore)
    if (kingCapturedInOne(snapshot)) {
      standardScore = -ScoreProvider.KingCatchableInOne
    }
    if(standardScore == 0 || standardScore.abs != ScoreProvider.PossibleWinInOne) {
      //println(scoreBlackCordon())
      val scoreErroneousBarricade: (Int, Int) = scoreWrongBarricade()
      standardScore -= scoreBlackCordon() - scoreErroneousBarricade._2
      standardScore += scoreTower() - scoreErroneousBarricade._1
    }
    standardScore
  }

  def computeAdvancedScore(snapshot: GameSnapshot): Int = {
    var advancedScore: Int = computeStandardScore(snapshot)
    //println("INNER STANDARD: " + advancedScore)

    if(advancedScore == 0 || advancedScore.abs != ScoreProvider.PossibleWinInOne) {
      val scoreRowOrColumnFree: (Int, Int) = scoreOwnerFirstLastThreeRowsOrColumns()
      advancedScore -= scoreLastPawnMovedCatchableInOne(snapshot) + scoreRowOrColumnFree._2
      advancedScore += scoreLastPawnMovedCatchableInOne(snapshot) + scoreRowOrColumnFree._1
    }
    advancedScore
  }

  /*
  def computeWhiteBetterPositions(gameSnapshot: GameSnapshot): Int = {

    //println("***********")
    //println("White King In Throne: " + scoreKingOnThrone())
    //println("White Tower: " + scoreTower())
    //println("King in Free Row or Column: " + scoreKingIsInFreeRowOrColumn())
    //println("White Captured Black: " + scoreCapturedBlack(gameSnapshot))
    //println("Last Black Moved capture In One: " + scoreBlackOnKingDiagonal())
    //println("***********")

    scoreKingOnThrone() +
    scoreKingIsInFreeRowOrColumn() +
    scoreCapturedBlack(gameSnapshot) +
    scoreTower() +
    scoreLastPawnMovedCatchableInOne(gameSnapshot)
  }

  def computeBlackBetterPositions(gameSnapshot: GameSnapshot): Int = {

    //println("***********")
    //println("Black Surround The King: " + scoreBlackSurroundTheKing())
    //println("Black Captured White: " + scoreCapturedWhite(gameSnapshot))
    //println("Black Cordon: " + scoreBlackCordon())
    //println("Black On King Diagonal: " + scoreBlackOnKingDiagonal())
    //println("Last White Moved capture In One: " + scoreBlackOnKingDiagonal())
    //println("***********")

    scoreBlackSurroundTheKing() +
      scoreBlackOnKingDiagonal() +
      scoreCapturedWhite(gameSnapshot) +
      scoreBlackCordon() +
      scoreLastPawnMovedCatchableInOne(gameSnapshot)
  }
  */
  /**
   * White and Black score
   * */

  def scoreOwnerFirstLastThreeRowsOrColumns(): (Int, Int) = {
    val boardTranspose = board.rows.toList.transpose
    val listRowsAndColumns: Seq[Seq[BoardCell]] = board.rows.take(3).toList ::: board.rows.takeRight(3).toList ::: boardTranspose.take(3) ::: boardTranspose.takeRight(3)
    var whiteScore = 0
    var blackScore = 0

    listRowsAndColumns.foreach(
      isRowOrColumnOwner(_) match {
        case Piece.WhiteKing => whiteScore += ScoreProvider.FirstLastRowOrColumnOwnedByKing
        case Piece.WhitePawn => whiteScore += ScoreProvider.FirstLastRowOrColumnOwnedByWhite
        case Piece.BlackPawn => blackScore += ScoreProvider.FirstLastRowOrColumnOwnedByBlack
        case _ =>
      }
    )
    (whiteScore, blackScore)
  }

  def scoreWrongBarricade(): (Int, Int) = {
    var whiteScore = 0
    var blackScore = 0
    board.cornerCoordinates.flatMap(c => getOrthogonalCoords(c)).filter(p => p._2 != Option.empty).grouped(2).foreach {
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get)).equals(Piece.BlackPawn) => blackScore += ScoreProvider.WrongCordon
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get)).equals(Piece.WhitePawn) => whiteScore += ScoreProvider.WrongCordon
      case _ =>
    }
    (whiteScore, blackScore)
  }


  /**
   * White score
   */

  def scoreKingOnThrone(): Int = {
    if (kingCoord.equals(board.centerCoordinates)) ScoreProvider.KingOnThrone
    else if (quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord)) == 0) ScoreProvider.KingDistanceToCornerDividend
    else ScoreProvider.KingDistanceToCornerDividend / quadraticDistanceBetweenCells(kingCoord, findCloserCorner(kingCoord))
  }

  def scoreTower(): Int = {
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

  def scoreCapturedBlack(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedBlacks * ScoreProvider.BlackCaptured

  def scoreKingIsInFreeRowOrColumn(): Int = {
    val rowWhithoutKing = kingOrthogonalCells(OrthogonalDirection.Right) ++ kingOrthogonalCells(OrthogonalDirection.Left)
    val columnWhithoutKing = kingOrthogonalCells(OrthogonalDirection.Up) ++ kingOrthogonalCells(OrthogonalDirection.Down)

    def _scoreKingIsInFreeRowOrColumn(): Int = (rowWhithoutKing, columnWhithoutKing) match {
      case (row, column) if isSequenceFreeCells(row) && isSequenceFreeCells(column) => ScoreProvider.RowAndColumnOwnedKing
      case (row, _) if isSequenceFreeCells(row) => ScoreProvider.RowOrColumnOwnedKing
      case (_, column) if isSequenceFreeCells(column) => ScoreProvider.RowOrColumnOwnedKing
      case _ => 0
    }

    _scoreKingIsInFreeRowOrColumn()
  }

  def scoreLastPawnMovedCatchableInOne(gameSnapshot: GameSnapshot): Int = {

    val lastMove = gameSnapshot.getLastMove

    if(lastMove.nonEmpty) {
      val adjOrthogonalCells = mappingOrthogonalToAdjacent(board.orthogonalCells(gameSnapshot.getLastMove.get.to))

      if (gameSnapshot.getPlayerToMove.equals(Player.Black) && checkOrthogonalEmptyCells(Piece.BlackPawn, adjOrthogonalCells))
        ScoreProvider.LastWhiteMovedCatchableInOne
      else if (gameSnapshot.getPlayerToMove.equals(Player.White) && checkOrthogonalEmptyCells(Piece.WhitePawn, adjOrthogonalCells))
        ScoreProvider.LastBlackMovedCatchableInOne
      else 0
    }
    else 0
  }

  def kingAdjacentToCorner(): Boolean = kingAdjacentCells.values.filter(_.nonEmpty).map(_.get.getCoordinate).exists(cornerCoordinates.contains(_))

  def kingToCornerInOne(): Boolean = {
    var returnValue = false
    for (i <- kingOrthogonalCells.keys)
      if (kingOrthogonalCells.isDefinedAt(i) && kingOrthogonalCells(i).nonEmpty &&
        cornerCoordinates.contains(kingOrthogonalCells(i).last.getCoordinate) &&
        (kingOrthogonalCells(i).count(_.getPiece.equals(Piece.Empty)) == kingOrthogonalCells(i).size))
        returnValue = true
    returnValue
  }

  def wrongBarricadeType(coord1: BoardCell, coord2: BoardCell): Piece.Value = (coord1.getPiece, coord2.getPiece) match {
    case (Piece.BlackPawn, Piece.BlackPawn) => Piece.BlackPawn
    case (Piece.WhitePawn, Piece.WhitePawn) => Piece.WhitePawn
    case _ => Piece.Empty
  }

  /**
   * Black score
   */
  def scoreCapturedWhite(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedWhites * ScoreProvider.WhiteCaptured

  def scoreBlackSurroundTheKing(): Int = {
    var score = 0
    for (cell <- kingAdjacentCells.values.filter(c => c.nonEmpty && c.get.getPiece.equals(Piece.BlackPawn))) {
      if(!checkOrthogonalEmptyCells(cell.get.getPiece, mappingOrthogonalToAdjacent(board.orthogonalCells(cell.get.getCoordinate))))
        score += ScoreProvider.BlackNearKing
    }
    score
    //kingAdjacentCells.values.count(c => c.nonEmpty && c.get.getPiece.equals(Piece.BlackPawn)) * ScoreProvider.BlackNearKing
  }

  def scoreBlackOnKingDiagonal(): Int = {
    var score = 0
    findNearBlacks(kingCoord).filter(c => (c.x != kingCoord.x) && (c.y != kingCoord.y)).foreach(c => board.getCell(c).getPiece match {
      case Piece.BlackPawn => score += ScoreProvider.BlackOnDiagonalKing
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

    listCordons.foreach({score += checkCorrectCordon(_)})

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

  def findNearBlacks(c: Coordinate): Seq[Coordinate] = {

    List(Coordinate(c.x + 1, c.y), Coordinate(c.x, c.y + 1),
      Coordinate(c.x - 1, c.y), Coordinate(c.x, c.y - 1),
      Coordinate(c.x - 1, c.y - 1), Coordinate(c.x + 1, c.y + 1),
      Coordinate(c.x - 1, c.y + 1), Coordinate(c.x + 1, c.y - 1))
      .filter(coord => coord.x <= boardSize && coord.x >= 1 &&
        coord.y <= boardSize && coord.y >= 1)
      .map(board.getCell).filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate)
  }

  def checkCorrectCordon(cordon: Seq[Coordinate]): Int = {
    if(isCircleCordon(cordon))
      checkCircleCordon(cordon)
    else checkNotCircleCordon(cordon)
  }

  def checkCircleCordon(cordon: Seq[Coordinate]): Int = {
    val (_,inn) = splitBoardWithCircleCordon(cordon)
    if(isCorrectCircleCordon(inn))
      cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon +
        + inn.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) * ScoreProvider.WhiteInsideCordon
    else
      cordon.size * ScoreProvider.PawnInCordon - ScoreProvider.WrongCordon
  }

  def checkNotCircleCordon(cordon: Seq[Coordinate]): Int = {
    if(isCorrectNotCircleCordon(cordon)) {
      var sequences: (Seq[BoardCell],Seq[BoardCell]) = (Seq.empty,Seq.empty)

      if (isHorizontalCordon(cordon))
        sequences = splitBoardWithHorizontalCordon(cordon)
      else
        sequences = splitBoardWithVerticalCordon(cordon)

      if (controlEmptyPortion(sequences._1) || controlEmptyPortion(sequences._2)) {
        cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon
      }
      else
        if(sequences._1.map(_.getCoordinate).contains(board.centerCoordinates))
          (cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon) -
            - (sequences._1.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) * ScoreProvider.WhitePawnOuterCordon)
        else
          (cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon) -
            - (sequences._2.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) * ScoreProvider.WhitePawnOuterCordon)
    } else
      0
  }

  def isCorrectCircleCordon(innerCells: Seq[BoardCell]): Boolean = innerCells.count(_.getPiece.equals(Piece.Empty)) == 0 ||
    innerCells.count(_.getPiece.equals(Piece.WhitePawn)) > 3 || innerCells.exists(_.getPiece.equals(Piece.WhiteKing))

  def isCorrectNotCircleCordon(cordon: Seq[Coordinate]): Boolean =
    cordon.filter(isOnAnySides(_)).map(getCurrentSide).filter(_.nonEmpty).map(_.get).distinct.size >= 2

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

  def isCircleCordon( cordon: Seq[Coordinate]): Boolean =
    cordon.count(findNearBlacks(_).count(cordon.contains(_)) >= 2) == cordon.size

  def isHorizontalCordon( cordon: Seq[Coordinate]): Boolean =
    cordon.filter(isOnAnySides(_))
      .map(getCurrentSide)
      .filter(_.nonEmpty)
      .map(_.get)
      .distinct
      .count(c => c.equals(OrthogonalDirection.Left)  || c.equals(OrthogonalDirection.Right)) >= 2


  /**
   * UTILS METHODS
   */

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
      returnValue = checkOrthogonalEmptyCells(Piece.BlackPawn, kingAdjacentCells)
    returnValue
  }

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

  private def controlEmptyPortion(sequence: Seq[BoardCell]): Boolean =
    sequence.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) == 0

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

  def checkOrthogonalEmptyCells(piece: Piece.Value, adjOrthogonalCells: Map[OrthogonalDirection, Option[BoardCell]]): Boolean = {
    var returnValue = false

    for (i <- adjOrthogonalCells.keys) {
      if(adjOrthogonalCells.isDefinedAt(i) && adjOrthogonalCells(i).nonEmpty &&
        adjOrthogonalCells(i).get.getPiece.equals(piece) &&
        adjOrthogonalCells(oppositeOrthogonalDirection(i)).nonEmpty &&
        gamePossibleMoves.map(_.to).contains(adjOrthogonalCells(oppositeOrthogonalDirection(i)).get.getCoordinate))
        returnValue = true
    }
    returnValue
  }

  def getSpecificOrthogonalCell(coord: Coordinate, orthogonalDirection: OrthogonalDirection, localBoard: Board ):Seq[BoardCell] = {
    val map = localBoard.orthogonalCells(coord)
    if( map.keySet.contains(orthogonalDirection)){
      return map(orthogonalDirection)
    }
    Seq.empty
  }

  private def samePlayerPawns(l: Seq[BoardCell], p: Piece.Val): Boolean =
    l.nonEmpty && l.size.equals(l.count(_.getPiece.equals(p)))

  def isItDistantFromCornerOf(coord: Coordinate, distance: Int): Boolean = {

    @scala.annotation.tailrec
    def _isItDistantFromTheCornerOf(coord: Coordinate, distance: Int, cornerCoord: List[Coordinate]): Boolean = cornerCoord match {
      case Nil => false
      case h :: _ if quadraticDistanceBetweenCells(h, coord) == distance => true
      case _ :: t => _isItDistantFromTheCornerOf(coord, distance, t)
    }

    _isItDistantFromTheCornerOf(coord, distance, cornerCoordinates)
  }

  //Quadratic distance
  def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate): Int = (scala.math.pow(start.x - end.x, 2) + scala.math.pow(start.y - end.y, 2)).toInt

  def findCloserCorner(coord: Coordinate): Coordinate = {
    @scala.annotation.tailrec
    def _getCloserCorner(closerCornerCord: Coordinate, closerDist: Int, cells: Seq[Coordinate] = cornerCoordinates): Coordinate = cells match {
      case Nil => closerCornerCord
      case h :: t =>
        val distCoordFromCorner = quadraticDistanceBetweenCells(h, coord)
        val (newCloserCorner, newCloserDist) = if (distCoordFromCorner > closerDist) (closerCornerCord, closerDist) else (h, distCoordFromCorner)
        _getCloserCorner(newCloserCorner, newCloserDist, t)
    }

    _getCloserCorner(Coordinate(1, 1), Int.MaxValue)
  }

  def isSequenceFreeCells(seq: Seq[BoardCell]): Boolean =
    seq.count(boardCell => !boardCell.getPiece.equals(Piece.Empty)) == 0

  private def oppositeOrthogonalDirection(orthogonalDirection: OrthogonalDirection): OrthogonalDirection = orthogonalDirection match {
    case OrthogonalDirection.Up => OrthogonalDirection.Down
    case OrthogonalDirection.Right => OrthogonalDirection.Left
    case OrthogonalDirection.Down => OrthogonalDirection.Up
    case OrthogonalDirection.Left => OrthogonalDirection.Right
  }

  private def mappingOrthogonalToAdjacent(orthogonal: Map[OrthogonalDirection, List[BoardCell]]): Map[OrthogonalDirection, Option[BoardCell]] = {
    orthogonal.map { case (k, v) => (k, v.take(1)) }
      .map { case (k, v) => if (v.nonEmpty) (k, Option(v.head)) else (k, Option.empty) }
  }


}


object blabla extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val prolog: ParserProlog = ParserPrologImpl(THEORY)
  var snapshot: GameSnapshot = _
  var game: (Player.Val, Player.Val, Board, Int) = _
  game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
  snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

  /*
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,5)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(4,8)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,8)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(4,6)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,8), Coordinate(4,11)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(5,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(2,11)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,9)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,10), Coordinate(1,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,9), Coordinate(9,9)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(8,2)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(8,6)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,6), Coordinate(10,4)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,4), Coordinate(11,3)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,7), Coordinate(8,9)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,3), Coordinate(9,3)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,9), Coordinate(9,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,8), Coordinate(10,8)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(9,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,8), Coordinate(10,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,7), Coordinate(9,9)))*/


/*
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(3,2)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,3)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,3)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(7,6)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(6,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,2)))

 */



  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(3,2)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(2,3)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(5,8)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,10), Coordinate(9,10)))
  snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,7)))

  EvaluationFunction.usefulValues(snapshot)


  println(snapshot.getBoard.consoleRepresentation)



  println(EvaluationFunction.computeNewcomerScore(snapshot))
  println(EvaluationFunction.scoreBlackCordon())
  //println(EvaluationFunction.computeStandardScore())
  //println(EvaluationFunction.cordons)

  //println(snapshot.getBoard.consoleRepresentation)

  //println("Newcomer: " + EvaluationFunction.computeNewcomerScore(snapshot))
  //println("Standard: " + EvaluationFunction.computeStandardScore(snapshot))
  //println("Advanced: " + EvaluationFunction.computeAdvancedScore(snapshot))


}
