package actor_ia

import model.GameSnapshot.GameSnapshotImpl
import model.{GameSnapshot, GameVariant, ParserProlog, ParserPrologImpl, Piece, Player, TheoryGame}
import utils.BoardGame.BoardCell
import utils.{Coordinate, Move}

object MoveGenerator {

  def gamePossibleMoves(gameSnapshot: GameSnapshot): List[Move] = {
    def _moves(cell: BoardCell): List[Move] = {
      gameSnapshot.getBoard.orthogonalCells(cell.getCoordinate)
        .flatMap(_cutAfterPiece(_, cell))
    }

    def _cutAfterPiece(sequence: List[BoardCell], cell: BoardCell): List[Move] =
      sequence.takeWhile(_.getPiece.equals(Piece.Empty))
        .filter(_filterIfPawn(_, cell))
        .map(c => Move(cell.getCoordinate, c.getCoordinate))

    def _filterIfPawn(cellToInspect: BoardCell, movingCell: BoardCell): Boolean = {
      (!movingCell.getPiece.equals(Piece.WhiteKing) && !gameSnapshot.getBoard.specialCoordinates.contains(cellToInspect.getCoordinate)) ||
        movingCell.getPiece.equals(Piece.WhiteKing)
    }

    gameSnapshot.getBoard.rows.flatMap(_.flatMap(c =>
      if (isOwner(c.getPiece, gameSnapshot.getPlayerToMove))
        _moves(c)
      else List.empty
    ))
  }.toList

  def makeMove(gameSnapshot: GameSnapshot, move: Move): GameSnapshot = {
    def _move(): Unit = {
      gameSnapshot.getBoard.setCell(BoardCell(move.to, gameSnapshot.getBoard.getCell(move.from).getPiece))
      gameSnapshot.getBoard.setCell(BoardCell(move.from, Piece.Empty))
    }

    def _checkCaptures(adjacentCells: List[BoardCell]): List[Coordinate] = adjacentCells match {
      case l if l.size < 2 => Nil
      case h :: t
        if (!isOwner(h.getPiece, gameSnapshot.getPlayerToMove) && !h.getPiece.equals(Piece.WhiteKing) && !h.getPiece.equals(Piece.Empty)) &&
        (isOwner(t.head.getPiece, gameSnapshot.getPlayerToMove) || gameSnapshot.getBoard.specialCoordinates.contains(t.head.getCoordinate))
        => List(h.getCoordinate)
      case _ => Nil
    }

    def _incrementCapturedPieces(piecesCaptured: Int): (Int, Int) = gameSnapshot.getPlayerToMove match {
      case Player.Black => (gameSnapshot.getNumberCapturedBlacks, gameSnapshot.getNumberCapturedWhites  + piecesCaptured)
      case Player.White => (gameSnapshot.getNumberCapturedBlacks + piecesCaptured, gameSnapshot.getNumberCapturedWhites)
      case _ => null
    }

    def switchPlayer(): Player.Val = gameSnapshot.getPlayerToMove match {
      case Player.Black => Player.White
      case Player.White => Player.Black
      case _ => Player.None
    }

    def checkWinner(): Player.Val = (checkVictory(), checkDraw()) match {
      case (true, false) => gameSnapshot.getPlayerToMove
      case (false, true) => Player.Draw
      case _ => Player.None
    }

    def checkVictory(): Boolean = (gameSnapshot.getVariant, gameSnapshot.getPlayerToMove) match{
      case (GameVariant.Hnefatafl | GameVariant.Tawlbwrdd, Player.Black) => checkBlackBigBoardVictory()
      case (GameVariant.Brandubh | GameVariant.Tablut , Player.Black) => checkBlackSmallBoardVictory()
      case _ => gameSnapshot.getBoard.cornerCoordinates.contains(move.to)
    }

    def checkDraw(): Boolean = {
      gamePossibleMoves(GameSnapshot(gameSnapshot.getVariant, switchPlayer(), Player.None, gameSnapshot.getBoard, Option(move), 0, 0)).isEmpty
    }

    def findKing():Coordinate = {
      gameSnapshot.getBoard.rows.flatten.filter(_.getPiece.equals(Piece.WhiteKing)).head.getCoordinate
    }

    def checkBlackBigBoardVictory(): Boolean = {
      val adjacentCells = gameSnapshot.getBoard.orthogonalCells(findKing())

      fourOrThreeSideCondition(adjacentCells)
    }

    def checkBlackSmallBoardVictory(): Boolean = {
      val kingCoord = findKing()
      val adjacentCells = gameSnapshot.getBoard.orthogonalCells(findKing())

      checkKingCapturedSmallBoard(kingCoord, adjacentCells)
    }

    def checkKingCapturedSmallBoard(kingCoord: Coordinate, adjacentCells: List[List[BoardCell]]): Boolean = {
      if(kingOnThrone(kingCoord) || kingNextToThrone(kingCoord))
        fourOrThreeSideCondition(adjacentCells)
      else
        kingCapturedTwoSides(adjacentCells)
    }

    def kingOnThrone(kingCoord: Coordinate): Boolean =
      kingCoord.equals(gameSnapshot.getBoard.specialCoordinates.last)


    def kingNextToThrone(kingCoord: Coordinate): Boolean =
      gameSnapshot.getBoard.orthogonalCells(gameSnapshot.getBoard.centerCoordinates).map(_.head).contains(kingCoord)


    def fourOrThreeSideCondition(adjacentCells: List[List[BoardCell]]): Boolean =
      adjacentCells.filter(_.nonEmpty).map(_.head).count(cell => cell.getPiece.equals(Piece.BlackPawn) ||
        cell.getCoordinate.equals(gameSnapshot.getBoard.centerCoordinates)) == 4


    def kingCapturedTwoSides(adjacentCells: List[List[BoardCell]]): Boolean = adjacentCells.filter(_.nonEmpty).map(_.head).size match {
      case 4 => kingCapturedInLine(adjacentCells(1).head, adjacentCells(3).head) || kingCapturedInLine(adjacentCells.head.head, adjacentCells(2).head)
      case _ => false
    }


    def kingCapturedInLine(lineCells1: BoardCell, lineCells2: BoardCell): Boolean =
      lineCells1.getPiece.equals(Piece.BlackPawn) && lineCells2.getPiece.equals(Piece.BlackPawn)


    _move()
    val listCapturesCoordinate = gameSnapshot.getBoard.orthogonalCells(move.to).map(_.take(2)).flatMap(list => _checkCaptures(list))
    listCapturesCoordinate.foreach(c => gameSnapshot.getBoard.setCell(BoardCell(c, Piece.Empty)))
    val capturedPieces = _incrementCapturedPieces(listCapturesCoordinate.size)

    GameSnapshot(gameSnapshot.getVariant, switchPlayer(), checkWinner(), gameSnapshot.getBoard, Option(move), capturedPieces._1, capturedPieces._2)
  }

  private def isOwner(pawn: Piece.Value, player: Player.Value): Boolean = (pawn, player) match {
    case (Piece.WhitePawn, Player.White) => true
    case (Piece.WhiteKing, Player.White) => true
    case (Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

}

// TODO fare test sensati
object daicheva extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  var variant: GameVariant.Val = GameVariant.Brandubh
  var game = parserProlog.createGame(variant.toString().toLowerCase)

  /* pezzi normali deve fare 40
  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  println(MoveGenerator.gamePossibleMoves(snap).size)*/

  /* re deve fare 35
  game = parserProlog.makeLegitMove(Move(Coordinate(2, 4), Coordinate(2, 7)))
  game = parserProlog.makeLegitMove(Move(Coordinate(3, 4), Coordinate(3, 7)))
  game = parserProlog.makeLegitMove(Move(Coordinate(1, 4), Coordinate(1, 6)))
  game = parserProlog.makeLegitMove(Move(Coordinate(4, 4), Coordinate(1, 4)))
  game = parserProlog.makeLegitMove(Move(Coordinate(4, 6), Coordinate(2, 6)))
  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  println(MoveGenerator.gamePossibleMoves(snap).size)*/

  /* test coordinate ortogonali
  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  println(snap.getBoard.nOrthogonalCoordinates(Coordinate(4,4)))*/

  /* test tempo gamePossibleMoves
  val snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  val start = System.currentTimeMillis()
  MoveGenerator.gamePossibleMoves(snap)
  val stop = System.currentTimeMillis() - start
  println(stop)*/

  /* test move generator make move */
  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  val snap2 = MoveGenerator.makeMove(snap, Move(Coordinate(4,1),Coordinate(4,3)))
  println(snap2.getBoard)

  /* test board setcell */
  var b = snap.getBoard
  b.setCell(BoardCell(Coordinate(2,6), Piece.Empty))
  //println(b.getCell(Coordinate(2,6)))

  println(parserProlog.gamePossibleMoves())
  /*val start = System.currentTimeMillis()
  MoveGenerator.findKing(snap2)
  val stop = System.currentTimeMillis() - start
  println(stop)*/

}