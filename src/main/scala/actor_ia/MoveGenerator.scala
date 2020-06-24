package actor_ia

import model.GameSnapshot.GameSnapshotImpl
import model.{GameSnapshot, GameVariant, ParserProlog, ParserPrologImpl, Piece, Player, TheoryGame}
import utils.BoardGame.BoardCell
import utils.Coordinate

object MoveGenerator {

  def gamePossibleMoves(gameSnapshot: GameSnapshot): List[(Coordinate, Coordinate)] = {
    def _moves(cell: BoardCell): List[(Coordinate, Coordinate)] = {
      gameSnapshot.getBoard.orthogonalCells(cell.getCoordinate)
        .flatMap(_cutAfterPiece(_, cell))
    }

    def _cutAfterPiece(sequence: List[BoardCell], cell: BoardCell): List[(Coordinate, Coordinate)] =
      sequence.takeWhile(_.getPiece.equals(Piece.Empty))
        .filter(_filterIfPawn(_, cell))
        .map(c => (cell.getCoordinate, c.getCoordinate))

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

  def isOwner(pawn: Piece.Value, player: Player.Value): Boolean = (pawn, player) match {
    case (Piece.WhitePawn, Player.White) => true
    case (Piece.WhiteKing, Player.White) => true
    case (Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

  def makeMove(gameSnapshot: GameSnapshot, fromToCoordinate: (Coordinate,Coordinate)): GameSnapshot = {
    def _move(): Unit = {
      gameSnapshot.getBoard.setCell(BoardCell(fromToCoordinate._2, gameSnapshot.getBoard.getCell(fromToCoordinate._1).getPiece))
      gameSnapshot.getBoard.setCell(BoardCell(fromToCoordinate._1, Piece.Empty))
    }

    def _checkCaptures(adjacentCells: List[BoardCell]): List[Coordinate] = adjacentCells match {
      case l if l.size < 2 => Nil
      case h :: t
        if (!isOwner(h.getPiece, gameSnapshot.getPlayerToMove) && !h.getPiece.equals(Piece.WhiteKing)) &&
        (isOwner(t.head.getPiece, gameSnapshot.getPlayerToMove) || gameSnapshot.getBoard.specialCoordinates.contains(t.head.getCoordinate))
        => List(h.getCoordinate)
      case _ => Nil
    }

    def _incrementCapturedPieces(piecesCaptured: Int): (Int, Int) = gameSnapshot.getPlayerToMove match {
      case Player.Black => (gameSnapshot.getNumberCapturedBlacks + piecesCaptured, gameSnapshot.getNumberCapturedWhites)
      case Player.White => (gameSnapshot.getNumberCapturedBlacks, gameSnapshot.getNumberCapturedWhites + piecesCaptured)
      case _ => null
    }

    def switchPlayer(): Player.Val = gameSnapshot.getPlayerToMove match {
      case Player.Black => Player.White
      case Player.White => Player.Black
      case _ => Player.None
    }

    _move()
    val listCapturesCoordinate = gameSnapshot.getBoard.orthogonalCells(fromToCoordinate._2).map(_.take(2)).flatMap(list => _checkCaptures(list))
    listCapturesCoordinate.foreach(c => gameSnapshot.getBoard.setCell(BoardCell(c, Piece.Empty)))
    val capturedPieces = _incrementCapturedPieces(listCapturesCoordinate.size)
    GameSnapshot(gameSnapshot.getVariant, switchPlayer(), Player.None, gameSnapshot.getBoard, Option(fromToCoordinate), capturedPieces._1, capturedPieces._2)
  }

}

object daicheva extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  var variant: GameVariant.Val = GameVariant.Brandubh
  var game = parserProlog.createGame(variant.toString().toLowerCase)

  /* pezzi normali deve fare 40
  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  println(prova.gamePossibleMoves(snap).size)*/

  /* re deve fare 35*/
  /*
  game = parserProlog.makeLegitMove(Coordinate(2, 4), Coordinate(2, 7))
  game = parserProlog.makeLegitMove(Coordinate(3, 4), Coordinate(3, 7))
  game = parserProlog.makeLegitMove(Coordinate(1, 4), Coordinate(1, 6))
  game = parserProlog.makeLegitMove(Coordinate(4, 4), Coordinate(1, 4))
  game = parserProlog.makeLegitMove(Coordinate(4, 6), Coordinate(2, 6))

  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  println(MoveGenerator.gamePossibleMoves(snap).size)
  */

  var snap = GameSnapshotImpl(variant, game._1, game._2, game._3, Option.empty, 0, 0)
  val snap2 = MoveGenerator.makeMove(snap, (Coordinate(4,1),Coordinate(4,3)))
  println(snap2.getBoard)



}