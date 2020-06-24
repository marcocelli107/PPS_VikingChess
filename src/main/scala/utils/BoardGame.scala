package utils

import model.Piece

/**
 * Util class representing a 2D coordinate
 */

object Coordinate {
  val COORD_STRING: String = "p"
}

case class Coordinate(x: Int, y: Int)  {
  override def toString: String = Coordinate.COORD_STRING + "(" + x + "," + y + ")"
}

object BoardGame {

  trait BoardCell {

    /**
     * Gets piece in the cell
     *
     * @return
     *         the piece type
     */
    def getPiece: Piece.Val

    /**
     * Gets coordinate of the cell
     *
     * @return
     *         the coordinate
     */
    def getCoordinate: Coordinate

    /**
     * Returns a string representation of the board
     *
     * @return
     *         a string representation of the board
     */
    def toString: String

  }

  object BoardCell {

    val CELL_STRING: String = "c"

    def apply(coordinateCell: Coordinate, piece: Piece.Val): BoardCell = BoardCellImpl(coordinateCell, piece)

    case class BoardCellImpl(private val coordinateCell: Coordinate, private val pieceCell: Piece.Val) extends BoardCell {

      override def getPiece: Piece.Val = pieceCell

      override def getCoordinate: Coordinate = coordinateCell

      override def toString: String = BoardCell.CELL_STRING + "(" + coordinateCell.toString + "," + pieceCell.pieceString + ")"
    }

  }

  trait Board {
    /**
     * Defines board's cells list.
     */
    def rows: Seq[Seq[BoardCell]]

    /**
     * Defines size of board's side.
     */
    def size: Int

    /**
     * Gets a cell in the board from a coordinate.
     */
    def getCell(coordinate: Coordinate): BoardCell

    def toString: String

    def orthogonalCells(coordinate: Coordinate): List[List[BoardCell]]

    def specialCoordinates: List[Coordinate]
  }

  object Board {

    def apply(cells: Seq[Seq[BoardCell]]): Board = BoardImpl(cells)

    case class BoardImpl(private val allCells: Seq[Seq[BoardCell]]) extends Board {

      override def rows: Seq[Seq[BoardCell]] = allCells

      override def size: Int = allCells.length

      override def getCell(coordinate: Coordinate): BoardCell = allCells (coordinate.x - 1) (coordinate.y - 1)

      override def equals(obj: Any): Boolean = this.rows.equals(obj.asInstanceOf[Board].rows)

      override def toString: String = rows.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")

      override def orthogonalCells(coordinate: Coordinate): List[List[BoardCell]] =
        List(upCells(coordinate), rightCells(coordinate), downCells(coordinate), leftCells(coordinate))

      private def upCells(coordinate: Coordinate): List[BoardCell] =
        (1 until coordinate.x).toList.map(x => getCell(Coordinate(x, coordinate.y))).reverse

      private def rightCells(coordinate: Coordinate): List[BoardCell] =
        rows(coordinate.x - 1).takeRight(size - coordinate.y).toList

      private def downCells(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x + 1 to size).toList.map(x => getCell(Coordinate(x, coordinate.y)))

      private def leftCells(coordinate: Coordinate): List[BoardCell] =
        rows(coordinate.x - 1).take(coordinate.y - 1).toList.reverse

      override def specialCoordinates: List[Coordinate] =
        List(Coordinate(1, 1), Coordinate(1, size), Coordinate(size, 1),
          Coordinate(size, size), Coordinate(size / 2 + 1, size / 2 + 1))

    }

  }

}