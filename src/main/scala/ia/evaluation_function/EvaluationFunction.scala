package ia.evaluation_function

import model.game.BoardGame.OrthogonalDirection.OrthogonalDirection
import model.game.BoardGame.{Board, BoardCell, OrthogonalDirection}
import model.game.Level.Level
import model.game.Piece.Piece
import model.game.{Coordinate, GameSnapshot, GameVariant, Level, Move, MoveGenerator, Piece, Player}
import scala.collection.immutable.HashMap

object EvaluationFunction {

  private var boardSize: Int = _
  private var kingCoordinate: Coordinate = _
  private var cornerCoordinates: List[Coordinate] = _
  private var centralCoordinate: Coordinate = _
  private var kingOrthogonalCells: Map[OrthogonalDirection, List[BoardCell]] = _
  private var kingAdjacentCells: Map[OrthogonalDirection, Option[BoardCell]] = _
  private var blackCoordinates: Seq[Coordinate] = _
  private var whiteCoordinates: Seq[Coordinate] = _
  private var gamePossibleMoves: Seq[Move] = _
  private var board: Board = _


  /**
    * Initializes utils variables for evaluation function.
    *
    * @param gameSnapshot
    *       current snapshot used to initialize variables.
    */
  def usefulValues(gameSnapshot: GameSnapshot): Unit = {
    board = gameSnapshot.getBoard
    boardSize = board.size

    kingCoordinate = MoveGenerator.findKing(board)
    kingOrthogonalCells = board.orthogonalCells(kingCoordinate)
    kingAdjacentCells = mappingOrthogonalToAdjacent(kingOrthogonalCells)

    cornerCoordinates = board.cornerCoordinates
    centralCoordinate = board.centerCoordinates

    blackCoordinates = board.rows.flatten
      .filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate).toList

    whiteCoordinates = board.rows.flatten
      .filter(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing))
      .map(_.getCoordinate).toList

    gamePossibleMoves = MoveGenerator.gamePossibleMoves(gameSnapshot)
  }

  /** Returns final score.
    *
    * @param snapshot
    *                 current snapshot to evaluate.
    * @param levelIA
    *                difficulty chosen.
    *
    * @return total score int
    */
  def score(snapshot: GameSnapshot, levelIA: Level): Int = snapshot.getWinner match {
    case Player.Black => -ScoreProvider.BlackWin
    case Player.White => ScoreProvider.WhiteWin
    case Player.Draw => ScoreProvider.Draw
    case _ => computeScore(snapshot, levelIA)
  }

  /** Selects score computation according to difficulty.
    *
    * @param gameSnapshot
    *                 current snapshot to evaluate.
    * @param levelIA
    *                difficulty chosen.
    *
    * @return total score int
    */
  def computeScore(gameSnapshot: GameSnapshot, levelIA: Level): Int = {
    usefulValues(gameSnapshot)
    var score: Int = 0

    levelIA match {
      case Level.Newcomer => score = computeNewcomerScore(gameSnapshot)
      case Level.Standard => score = computeStandardScore(gameSnapshot)
      case _ => score = computeAdvancedScore(gameSnapshot)
    }
    score
  }

  /** Returns score computation for difficulty Newcomer.
    *
    * @param snapshot
    *                 current snapshot to evaluate.
    *
    * @return newcomer score
    */
  def computeNewcomerScore(snapshot: GameSnapshot): Int = {
    var newcomerScore: Int = 0

    if ((kingAdjacentToCorner || kingToCornerInOne(snapshot) || kingNearToCornerInOne(snapshot)) &&
      snapshot.getPlayerToMove.equals(Player.White))
      newcomerScore += ScoreProvider.KingEscapeToCorner

    if ((kingCapturedInOne(snapshot) ||
        ((kingToCornerInOne(snapshot) || kingNearToCornerInOne(snapshot)) && kingIsLockable(snapshot)))
        && snapshot.getPlayerToMove.equals(Player.Black))
      newcomerScore -= ScoreProvider.KingCatchableInOne

    if(newcomerScore == 0) {
      newcomerScore -= scoreBlackSurroundTheKing +
                        scoreBlackOnKingDiagonal +
                        scoreCapturedWhite(snapshot)
      newcomerScore += scoreKingOnThrone +
                        scoreKingIsInFreeRowOrColumn +
                        scoreCapturedBlack(snapshot)
    }
    newcomerScore
  }

  /** Returns score computation for difficulty Standard.
    *
    * @param snapshot
    *                 current snapshot to evaluate.
    *
    * @return standard score
    */
  def computeStandardScore(snapshot: GameSnapshot): Int = {
    var standardScore: Int = computeNewcomerScore(snapshot)

    if(standardScore == 0 || standardScore.abs != ScoreProvider.PossibleWinInOne) {
      val scoreErroneousBarricade: (Int, Int) = scoreWrongBarricade
      standardScore -= scoreBlackCordon - scoreErroneousBarricade._2
      standardScore += scoreTower - scoreErroneousBarricade._1
    }
    standardScore
  }

  /** Returns score computation for difficulty Advanced.
    *
    * @param snapshot
    *                 current snapshot to evaluate.
    *
    * @return advanced score
    */
  def computeAdvancedScore(snapshot: GameSnapshot): Int = {
    var advancedScore: Int = computeStandardScore(snapshot)
    if(advancedScore == 0 || advancedScore.abs != ScoreProvider.PossibleWinInOne) {
      val scoreRowOrColumnFree: (Int, Int) = scoreOwnerFirstLastThreeRowsOrColumns
      advancedScore -= scoreLastPawnMovedCatchableInOne(snapshot) + scoreRowOrColumnFree._2
      advancedScore += scoreLastPawnMovedCatchableInOne(snapshot) + scoreRowOrColumnFree._1
    }
    advancedScore
  }


  /** Checks if the king is near a corner.
    *
    * @return boolean
    */
  def kingAdjacentToCorner: Boolean = kingAdjacentCells.values
    .filter(_.nonEmpty)
    .map(_.get.getCoordinate).exists(cornerCoordinates.contains(_))

  /** Checks if the king can move near a corner.
    *
    * @param gameSnapshot
    *                     current snapshot to evaluate king's possible moves.
    *
    * @return boolean
    */
  def kingNearToCornerInOne(gameSnapshot: GameSnapshot): Boolean = {
    isKingPossibleToMove(gameSnapshot, getNearCornerCells)
  }

  /** Checks if the king can move to a corner.
    *
    * @param gameSnapshot
    *                     current snapshot to evaluate king's possible moves.
    *
    * @return boolean
    */
  def kingToCornerInOne(gameSnapshot: GameSnapshot): Boolean = {
    isKingPossibleToMove(gameSnapshot, cornerCoordinates)
  }

  /** Checks if the king is catchable.
    *
    * @param gameSnapshot
    *                   current snapshot to checking king capture.
    *
    * @return boolean
    */
  def kingCapturedInOne(gameSnapshot: GameSnapshot): Boolean = gameSnapshot.getVariant match {
    case GameVariant.Hnefatafl | GameVariant.Tawlbwrdd => kingCapturedInOneBigBoard()
    case _ => kingCapturedInOneSmallBoard(gameSnapshot)
  }

  /** Checks if the king is lockable.
    *
    * @param gameSnapshot
    *                   current snapshot to checking block.
    *
    * @return boolean
    */
  def kingIsLockable(gameSnapshot: GameSnapshot): Boolean = {
    val coordinateCornerToBlock = findCloserCorner(kingCoordinate)
    for (black <- blackCoordinates) {
      MoveGenerator.coordPossibleMoves(BoardCell(black, Piece.BlackPawn), gameSnapshot)
        .map(_.to).contains(getOrthogonalCoordinates(coordinateCornerToBlock).values.filter(_.nonEmpty).map(_.get))
      return true
    }
    false
  }

  /** Returns score according to who owns first and last three rows and columns.
    *
    * @return (Int, Int): (scoreWhite, scoreBlack)
    */
  def scoreOwnerFirstLastThreeRowsOrColumns: (Int, Int) = {
    var whiteScore = 0
    var blackScore = 0
    val boardTranspose = board.rows.toList.transpose
    val listRowsAndColumns: Seq[Seq[BoardCell]] = board.rows
      .take(3).toList ::: board.rows.takeRight(3).toList ::: boardTranspose.take(3) ::: boardTranspose.takeRight(3)

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

  /** Returns score for a wrong barricade.
    *
    * @return (Int, Int): (scoreWhite, scoreBlack)
    */
  def scoreWrongBarricade: (Int, Int) = {
    var whiteScore = 0
    var blackScore = 0
    board.cornerCoordinates.flatMap(c => getOrthogonalCoordinates(c)).filter(p => p._2 != Option.empty).grouped(2).foreach {
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get))
        .equals(Piece.BlackPawn) => blackScore += ScoreProvider.WrongCordon
      case h :: t if wrongBarricadeType(board.getCell(h._2.get),board.getCell(t.head._2.get))
        .equals(Piece.WhitePawn) => whiteScore += ScoreProvider.WrongCordon
      case _ =>
    }
    (whiteScore, blackScore)
  }

  /** Returns score for last pawn moved catchable in one.
    *
    * @param gameSnapshot
    *                     current snapshot to checking last move.
    *
    * @return Int: score capture last pawn moved
    */
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

  /**
    * WHITE SCORES
    */

  /** Returns score according to distance from throne.
    *
    * @return Int: scoreKingOnThrone
    */
  def scoreKingOnThrone: Int = {
    if(kingCoordinate.equals(board.centerCoordinates)) ScoreProvider.KingOnThrone
    else if(quadraticDistanceBetweenCells(kingCoordinate, findCloserCorner(kingCoordinate)) == 0)
      ScoreProvider.KingDistanceToCornerDividend
    else
      ScoreProvider.KingDistanceToCornerDividend / quadraticDistanceBetweenCells(kingCoordinate, findCloserCorner(kingCoordinate))
  }

  /** Returns score for a white tower.
    *
    * @return Int: scoreTower
    */
  def scoreTower: Int = {
    def _isSquare(list: List[BoardCell]): Boolean = {
      list.count(cell => cell.getPiece.equals(Piece.WhitePawn) || cell.getPiece.equals(Piece.WhiteKing)) == 3
    }

    var score: Double = 0

    for {
      coordinate <- whiteCoordinates
      i = coordinate.x
      j = coordinate.y
      if i < boardSize && j < boardSize
      if _isSquare(List(board.getCell(Coordinate(i, j + 1)),
        board.getCell(Coordinate(i + 1, j)),
        board.getCell(Coordinate(i + 1, j + 1))))
    } yield score += ScoreProvider.TowerCoefficient * quadraticDistanceBetweenCells(coordinate, centralCoordinate)

    score.toInt
  }

  /** Returns score for a captured black piece.
    *
    * @param gameSnapshot
    *                   current snapshot to checking number of blacks captured.
    *
    * @return Int: scoreCapture
    */
  def scoreCapturedBlack(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedBlacks * ScoreProvider.BlackCaptured

  /** Returns score for the row and column controlled from king.
    *
    * @return Int: score row, column owned
    */
  def scoreKingIsInFreeRowOrColumn: Int = {
    val rowWithoutKing = kingOrthogonalCells(OrthogonalDirection.Right) ++ kingOrthogonalCells(OrthogonalDirection.Left)
    val columnWithoutKing = kingOrthogonalCells(OrthogonalDirection.Up) ++ kingOrthogonalCells(OrthogonalDirection.Down)

    def _scoreKingIsInFreeRowOrColumn(): Int = (rowWithoutKing, columnWithoutKing) match {
      case (row, column) if isSequenceFreeCells(row) && isSequenceFreeCells(column) => ScoreProvider.RowAndColumnOwnedKing
      case (row, _) if isSequenceFreeCells(row) => ScoreProvider.RowOrColumnOwnedKing
      case (_, column) if isSequenceFreeCells(column) => ScoreProvider.RowOrColumnOwnedKing
      case _ => 0
    }
    _scoreKingIsInFreeRowOrColumn()
  }



  /**
    * BLACK SCORES
    */

  /** Returns score for a captured white piece.
    *
    * @param gameSnapshot
    *                   current snapshot to checking number of whites captured.
    *
    * @return Int: scoreCapture
    */
  def scoreCapturedWhite(gameSnapshot: GameSnapshot): Int = gameSnapshot.getNumberCapturedWhites * ScoreProvider.WhiteCaptured

  /** Returns score according to number of blacks surround to king.
    *
    * @return Int: scoreCapture
    */
  def scoreBlackSurroundTheKing: Int = {
    kingAdjacentCells.values.count(c => c.nonEmpty &&
      c.get.getPiece.equals(Piece.BlackPawn) &&
      !checkOrthogonalEmptyCells(c.get.getPiece, mappingOrthogonalToAdjacent(board.orthogonalCells(c.get.getCoordinate)))) *
      ScoreProvider.BlackNearKing
  }

  /** Returns score according to number of blacks on king's diagonals.
    *
    * @return score int
    */
  def scoreBlackOnKingDiagonal: Int = {
    var score = 0
    findNearBlacks(kingCoordinate).filter(c => (c.x != kingCoordinate.x) && (c.y != kingCoordinate.y)).foreach(c => board.getCell(c).getPiece match {
      case Piece.BlackPawn => score += ScoreProvider.BlackOnDiagonalKing
      case _ =>
    })
    score
  }

  /** Returns score for a black cordon.
    *
    * @return score int
    */
  def scoreBlackCordon: Int = {
    var cordons: Seq[Seq[Coordinate]] = Seq.empty

    blackCoordinates.foreach(b => {
      if (!cordons.flatten.contains(b))
        cordons :+= createCordon(b, Seq.empty)
    })
    val listCordons = cordons.distinct.filter(_.size >= 3)
    var score = 0

    listCordons.foreach({score += checkCorrectCordon(_)})

    score
  }

  private def isKingPossibleToMove(snapshot: GameSnapshot, listCoordinates: List[Coordinate]): Boolean =
    MoveGenerator.coordPossibleMoves(BoardCell(kingCoordinate,Piece.WhiteKing), snapshot).map(_.to).exists(listCoordinates.contains)

  private def wrongBarricadeType(firstCoordinate: BoardCell, secondCoordinate: BoardCell): Piece =
    (firstCoordinate.getPiece, secondCoordinate.getPiece) match {
      case (Piece.BlackPawn, Piece.BlackPawn) => Piece.BlackPawn
      case (Piece.WhitePawn, Piece.WhitePawn) => Piece.WhitePawn
      case _ => Piece.Empty
  }

  private def getNearCornerCells: List[Coordinate] =  board.cornerCoordinates.flatMap(board.orthogonalCells(_)).map(_._2).filter(_.nonEmpty).map(_.head).map(_.getCoordinate)

  private def createCordon(fromCoordinate: Coordinate, cordon: Seq[Coordinate]): Seq[Coordinate] = {

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

  private def isRedundant(coordinate: Coordinate, cordon: Seq[Coordinate]): Boolean = {
    val cycles = get3CycleCoordinates(coordinate)
    for (d <- OrthogonalDirection.values) {
      if (cycles(d).size == 2 && isSubList(cycles(d), cordon))
        return true
    }
    false
  }

  private def isSubList(subList: Seq[Any], list: Seq[Any]): Boolean = subList.forall(list.contains)

  private def get3CycleCoordinates(c: Coordinate): Map[OrthogonalDirection, List[Coordinate]] = {
    HashMap(
      OrthogonalDirection.Up ->
        List(getOrthogonalCoordinates(c)(OrthogonalDirection.Left),
          getOrthogonalCoordinates(c)(OrthogonalDirection.Up)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Right ->
        List(getOrthogonalCoordinates(c)(OrthogonalDirection.Up),
          getOrthogonalCoordinates(c)(OrthogonalDirection.Right)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Down ->
        List(getOrthogonalCoordinates(c)(OrthogonalDirection.Right),
          getOrthogonalCoordinates(c)(OrthogonalDirection.Down)).filter(_.nonEmpty).map(_.get),
      OrthogonalDirection.Left ->
        List(getOrthogonalCoordinates(c)(OrthogonalDirection.Down),
          getOrthogonalCoordinates(c)(OrthogonalDirection.Left)).filter(_.nonEmpty).map(_.get)
    )
  }

  private def getOrthogonalCoordinates(coordinate: Coordinate): Map[OrthogonalDirection, Option[Coordinate]] =
    HashMap(
      OrthogonalDirection.Up ->
        filterOutOfBoardCoordinate(Coordinate(coordinate.x - 1, coordinate.y)),
      OrthogonalDirection.Right ->
        filterOutOfBoardCoordinate(Coordinate(coordinate.x, coordinate.y + 1)),
      OrthogonalDirection.Down ->
        filterOutOfBoardCoordinate(Coordinate(coordinate.x + 1, coordinate.y)),
      OrthogonalDirection.Left ->
        filterOutOfBoardCoordinate(Coordinate(coordinate.x, coordinate.y - 1))
    )

  private def filterOutOfBoardCoordinate(coordinate: Coordinate): Option[Coordinate] =
    if (!outOfBoardCoordinate(coordinate))
      Option(coordinate)
    else
      Option.empty

  private def outOfBoardCoordinate(coordinate: Coordinate): Boolean =
    coordinate.x > boardSize || coordinate.x < 1 || coordinate.y > boardSize || coordinate.y < 1

  private def findNearBlacks(c: Coordinate): Seq[Coordinate] = {
    List(Coordinate(c.x + 1, c.y), Coordinate(c.x, c.y + 1),
      Coordinate(c.x - 1, c.y), Coordinate(c.x, c.y - 1),
      Coordinate(c.x - 1, c.y - 1), Coordinate(c.x + 1, c.y + 1),
      Coordinate(c.x - 1, c.y + 1), Coordinate(c.x + 1, c.y - 1))
      .filter(coord => coord.x <= boardSize && coord.x >= 1 &&
        coord.y <= boardSize && coord.y >= 1)
      .map(board.getCell).filter(_.getPiece.equals(Piece.BlackPawn)).map(_.getCoordinate)
  }

  private def checkCorrectCordon(cordon: Seq[Coordinate]): Int = {
    if(isCircleCordon(cordon))
      checkCircleCordon(cordon)
    else checkNotCircleCordon(cordon)
  }

  private def checkCircleCordon(cordon: Seq[Coordinate]): Int = {
    val (_,inn) = splitBoardWithCircleCordon(cordon)
    if(isCorrectCircleCordon(inn))
      cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon +
        + inn.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) * ScoreProvider.WhiteInsideCordon
    else
      cordon.size * ScoreProvider.PawnInCordon - ScoreProvider.WrongCordon
  }

  private def checkNotCircleCordon(cordon: Seq[Coordinate]): Int = {
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
        outalierPenalty(sequences._1, cordon)
      else
        outalierPenalty(sequences._2, cordon)
    } else
      0
  }

  private def outalierPenalty(seq: Seq[BoardCell], cordon: Seq[Coordinate]):Int =
    (cordon.size * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon) - (seq.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) * ScoreProvider.WhitePawnOuterCordon)

  private def isCorrectCircleCordon(innerCells: Seq[BoardCell]): Boolean = innerCells.count(_.getPiece.equals(Piece.Empty)) == 0 ||
    innerCells.count(_.getPiece.equals(Piece.WhitePawn)) > 3 || innerCells.exists(_.getPiece.equals(Piece.WhiteKing))

  private def isCorrectNotCircleCordon(cordon: Seq[Coordinate]): Boolean =
    cordon.filter(isOnAnySides(_)).map(getCurrentSide).filter(_.nonEmpty).map(_.get).distinct.size >= 2

  private def splitBoardWithCircleCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {

    @scala.annotation.tailrec
    def _splitBoardWithCircleCordon(cordon: Seq[Coordinate], output: Seq[(Seq[BoardCell],Seq[BoardCell])]): Seq[(Seq[BoardCell],Seq[BoardCell])] =
      cordon match {
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

  private def splitBoardWithHorizontalCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {
    splitBoardWithNotCircleCordon(cordon,OrthogonalDirection.Up)  }

  private def splitBoardWithVerticalCordon(cordon: Seq[Coordinate]): (Seq[BoardCell], Seq[BoardCell]) = {
    splitBoardWithNotCircleCordon(cordon,OrthogonalDirection.Left)
  }

  private def splitBoardWithNotCircleCordon(cordon: Seq[Coordinate], orthogonalDirection: OrthogonalDirection): (Seq[BoardCell], Seq[BoardCell]) = {
    val leftCordonCells: Seq[BoardCell] = cordon.flatMap(getSpecificOrthogonalCell(_, orthogonalDirection, board)).toList
    val rightCordonCells: Seq[BoardCell] = board.rows.flatten.diff(leftCordonCells).filter(cell => !cordon.contains( cell.getCoordinate)).toList
    (leftCordonCells, rightCordonCells)
  }

  private def isCircleCordon( cordon: Seq[Coordinate]): Boolean =
    cordon.count(findNearBlacks(_).count(cordon.contains(_)) >= 2) == cordon.size

  private def isHorizontalCordon( cordon: Seq[Coordinate]): Boolean =
    cordon.filter(isOnAnySides(_))
      .map(getCurrentSide)
      .filter(_.nonEmpty)
      .map(_.get)
      .distinct
      .count(c => c.equals(OrthogonalDirection.Left)  || c.equals(OrthogonalDirection.Right)) >= 2

  private def kingCapturedInOneBigBoard(): Boolean = {
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

  private def kingCapturedInOneSmallBoard(gameSnapshot: GameSnapshot): Boolean = {
    var returnValue = false
    if (MoveGenerator.kingOnThrone(gameSnapshot, kingCoordinate) || MoveGenerator.kingNextToThrone(gameSnapshot, kingCoordinate)) {
      returnValue = kingCapturedInOneBigBoard()
    }
    else
      returnValue = checkOrthogonalEmptyCells(Piece.BlackPawn, kingAdjacentCells)
    returnValue
  }

  /**
   * No black piece can sit on a corner so no double sides.
   */
  private def getCurrentSide(coordinateOnSide: Coordinate): Option[OrthogonalDirection] = coordinateOnSide match {
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Up) => Option(OrthogonalDirection.Up)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Right) => Option(OrthogonalDirection.Right)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Down) => Option(OrthogonalDirection.Down)
    case coordinate if isOnSide(coordinate, OrthogonalDirection.Left) => Option(OrthogonalDirection.Left)
    case _ => Option.empty
  }

  private def controlEmptyPortion(sequence: Seq[BoardCell]): Boolean =
    sequence.count(c => c.getPiece.equals(Piece.WhitePawn) || c.getPiece.equals(Piece.WhiteKing)) == 0

  private def isOnSide(coordinate: Coordinate, side: OrthogonalDirection): Boolean = side match {
    case OrthogonalDirection.Up => coordinate.x == 1
    case OrthogonalDirection.Right => coordinate.y == boardSize
    case OrthogonalDirection.Down => coordinate.x == boardSize
    case OrthogonalDirection.Left => coordinate.y == 1
    case _ => false
  }

  private def isOnAnySides(coordinate: Coordinate, sides: Seq[OrthogonalDirection] = Seq(OrthogonalDirection.Up, OrthogonalDirection.Down, OrthogonalDirection.Right, OrthogonalDirection.Left)): Boolean =
    sides.exists(isOnSide(coordinate, _))

  private def isRowOrColumnOwner(cellsSeq: Seq[BoardCell]): Piece = {
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


  private def checkOrthogonalEmptyCells(piece: Piece, adjOrthogonalCells: Map[OrthogonalDirection, Option[BoardCell]]): Boolean = {
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

  private def getSpecificOrthogonalCell(coord: Coordinate, orthogonalDirection: OrthogonalDirection, localBoard: Board ):Seq[BoardCell] = {
    val map = localBoard.orthogonalCells(coord)
    if( map.keySet.contains(orthogonalDirection)){
      return map(orthogonalDirection)
    }
    Seq.empty
  }

  private def samePlayerPawns(l: Seq[BoardCell], p: Piece): Boolean =
    l.nonEmpty && l.size.equals(l.count(_.getPiece.equals(p)))

  private def quadraticDistanceBetweenCells(start: Coordinate, end: Coordinate): Int = (scala.math.pow(start.x - end.x, 2) + scala.math.pow(start.y - end.y, 2)).toInt

  private def findCloserCorner(coord: Coordinate): Coordinate = {
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

  private def isSequenceFreeCells(seq: Seq[BoardCell]): Boolean =
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
