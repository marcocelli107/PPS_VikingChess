package actor_ia

import model.{GameSnapshot, Piece, Player}
import utils.BoardGame.BoardCell
import utils.Coordinate

object GeneratorMoves {

  def getPossibleMoves(gameSnapshot: GameSnapshot): List[(Coordinate,Coordinate)] = {
    var listPossibleMoves: List[(Coordinate, Coordinate)] = List.empty

    def getMyMove(cell: BoardCell): List[(Coordinate,Coordinate)] = {
      val listCellMoves: List[(Coordinate, Coordinate)] = List.empty

      /*gameSnapshot.getBoard.rows.foreach(
        _.filter(c => c.getCoordinate.x.equals(cell.getCoordinate.x || c.getCoordinate.y.equals()))
          .filter(_.getPiece.equals(Piece.Empty))
      )*/


      listCellMoves
    }

    gameSnapshot.getBoard.rows.foreach(_.foreach(c => {
      if(isOwner(c.getPiece, gameSnapshot.getPlayerToMove))
        listPossibleMoves ++ getMyMove(c)
    }))

    listPossibleMoves
  }

  private def isOwner(pawn:Piece.Value, player: Player.Value): Boolean = ( pawn, player) match {
    case ( Piece.WhitePawn, Player.White) => true
    case ( Piece.WhiteKing, Player.White) => true
    case ( Piece.BlackPawn, Player.Black) => true
    case _ => false
  }
}
