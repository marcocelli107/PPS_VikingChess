package utils

import model.Piece.PieceType

case class Coordinate(x: Int, y: Int)

object BoardGame {

  trait BoardCell {
    /**
      * Coordinate of the cell
      */
    //def coordinates: Coordinate

    /**
      * Piece in the cell
      */
    //def piece: PieceType

    /**
      * Gets piece in the cell
      */
    def getPiece: PieceType

    /**
      * Gets coordinate of the cell
      */
    def getCoordinate: Coordinate
  }

  object BoardCell {

    def apply(coordinateCell: Coordinate, piece: PieceType): BoardCell = BoardCellImpl(coordinateCell, piece)

    case class BoardCellImpl(coordinateCell: Coordinate, pieceCell: PieceType) extends BoardCell {

      //override def coordinates: Coordinate = coordinateCell

      //override def piece: PieceType = pieceCell
      //private val coordinate: Coordinate = coordinateCell

      //private val piece: PieceType = pieceCell

      override def getPiece: PieceType = pieceCell

      override def getCoordinate: Coordinate = coordinateCell
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
  }

  object Board {

    def apply(cells: Seq[BoardCell]): Board = BoardImpl(cells)

    case class BoardImpl(allCells: Seq[BoardCell]) extends Board {

      override def cells: Seq[BoardCell] = allCells

      override def size: Int = Math.sqrt(allCells.length).toInt

      override def getCell(coordinate: Coordinate): BoardCell = allCells.filter(_.getCoordinate == coordinate).head

      override def equals(obj: Any): Boolean = this.cells.equals(obj.asInstanceOf[Board].cells)
    }
  }
}