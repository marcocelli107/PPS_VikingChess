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

  def isOwner(pawn: Piece.Value, player: Player.Value): Boolean = (pawn, player) match {
    case (Piece.WhitePawn, Player.White) => true
    case (Piece.WhiteKing, Player.White) => true
    case (Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

}

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

  /*
  println(MoveGenerator.gamePossibleMoves(snap).size)

  var b = snap.getBoard
  b.setCell(BoardCell(Coordinate(2,6), Piece.Empty))
  println(b.getCell(Coordinate(2,6)))
*/

}