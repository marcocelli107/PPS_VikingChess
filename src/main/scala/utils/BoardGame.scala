package utils

import model.Piece
import model.Piece.PieceType
import utils.BoardGame.{Board, BoardCell}

import scala.collection.mutable.ListBuffer

case class Coordinate(x: Int, y: Int)

object BoardGame {

  trait BoardCell {

    /**
     * Gets piece in the cell
     */
    def getPiece: PieceType

    /**
     * Gets coordinate of the cell
     */
    def getCoordinate: Coordinate

    def toString: String
  }

  object BoardCell {

    def apply(coordinateCell: Coordinate, piece: PieceType): BoardCell = BoardCellImpl(coordinateCell, piece)

    case class BoardCellImpl(private val coordinateCell: Coordinate, private val pieceCell: PieceType) extends BoardCell {

      override def getPiece: PieceType = pieceCell

      override def getCoordinate: Coordinate = coordinateCell

      override def toString: String = {
        var p: String = ""
        /* TODO USARE ENUM */
        if (pieceCell.equals(Piece.Void)) p = "e"
        if (pieceCell.equals(Piece.WhitePawn)) p = "wp"
        if (pieceCell.equals(Piece.BlackPawn)) p = "bp"
        if (pieceCell.equals(Piece.WhiteKing)) p = "wk"
        "cell(coord(" + coordinateCell.x + "," + coordinateCell.y + ")," + p + ")"
      }
    }

  }

  trait Board {
    /**
     * Defines board's cells list.
     */
    def cells: Seq[BoardCell]

    /**
     * Defines size of board's side.
     */
    def size: Int

    /**
     * Gets a cell in the board from a coordinate.
     */
    def getCell(coordinate: Coordinate): BoardCell

    def toString: String
  }

  object Board {

    def apply(cells: Seq[BoardCell]): Board = BoardImpl(cells)

    case class BoardImpl(private val allCells: Seq[BoardCell]) extends Board {

      override def cells: Seq[BoardCell] = allCells

      override def size: Int = Math.sqrt(allCells.length).toInt

      override def getCell(coordinate: Coordinate): BoardCell = allCells.filter(_.getCoordinate == coordinate).head

      override def equals(obj: Any): Boolean = this.cells.equals(obj.asInstanceOf[Board].cells)

      override def toString: String = cells.map(_.toString).grouped(size).toList.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
    }

  }

}

object prova extends App {
  var s: ListBuffer[BoardCell] = ListBuffer.empty
  for (x <- 1 to 11) {
    for (y <- 1 to 11) {
      s += BoardCell(Coordinate(x, y), Piece.Void)
    }
  }
  var b: Board = Board(s)
  println(b.toString)
}