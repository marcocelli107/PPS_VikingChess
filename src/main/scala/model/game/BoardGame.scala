package model.game

import model.game.BoardGame.DiagonalDirection.DiagonalDirection
import model.game.BoardGame.OrthogonalDirection.OrthogonalDirection
import model.game.Piece.Piece

import scala.collection.immutable.HashMap

/**
  * Util class representing a 2D coordinate
  */
object Coordinate {
  val COORD_STRING: String = "p"

  /**
    * Orders the coordinates according to rows.
    *
    * @return ordered coordinates.
    */
  implicit val coordinateOrdering: Ordering[Coordinate] = (a: Coordinate, b: Coordinate) => {
    a.x compare b.x match {
      case 0 => a.y compare b.y
      case c => c
    }
  }

}

case class Coordinate(x: Int, y: Int) {
  override def toString: String = Coordinate.COORD_STRING + "(" + x + "," + y + ")"
}

/**
  * Util class representing a move from coordinate to coordinate.
  */
case class Move(from: Coordinate, to: Coordinate) {
  override def toString: String = "move(from(" + from + ", to(" + to + "))"
}

object BoardGame {

  trait BoardCell {

    /**
      * Gets piece in the cell.
      *
      * @return
      *         the piece type
      */
    def getPiece: Piece

    /**
      * Gets coordinate of the cell.
      *
      * @return
      *        the coordinate
      */
    def getCoordinate: Coordinate

    /**
      * Returns a string representation of the board.
      *
      * @return
      *         a string representation of the board
      */
    def toString: String

  }

  object BoardCell {

    val CELL_STRING: String = "c"

    def apply(coordinateCell: Coordinate, piece: Piece): BoardCell = BoardCellImpl(coordinateCell, piece)

    case class BoardCellImpl(private val coordinateCell: Coordinate, private val pieceCell: Piece) extends BoardCell {

      override def getPiece: Piece = pieceCell

      override def getCoordinate: Coordinate = coordinateCell

      override def toString: String = BoardCell.CELL_STRING + "(" + coordinateCell.toString + "," + pieceCell.toString + ")"
    }

  }

  object OrthogonalDirection extends Enumeration {
    type OrthogonalDirection = Value
    val Up, Right, Down, Left = Value

    /**
      * Orders the orthogonal directions clockwise as up, right, down, left.
      *
      * @return ordered orthogonal directions.
      */
    implicit val orthogonalOrdering: Ordering[OrthogonalDirection] = (a: OrthogonalDirection, b: OrthogonalDirection) =>
      a.directionToOrder compare b.directionToOrder

    case class OrthogonalDirectionValue(direction: OrthogonalDirection) {
      def directionToOrder: Int = direction match {
        case OrthogonalDirection.Up => 1
        case OrthogonalDirection.Right => 2
        case OrthogonalDirection.Down => 3
        case _ => 4
      }
    }

    /**
      * Extracts implicitly direction value according to orthogonal direction.
      *
      * @param direction
      *               orthogonal direction to extract.
      *
      * @return value to int
      */
    implicit def directionValue(direction: OrthogonalDirection): OrthogonalDirectionValue = OrthogonalDirectionValue(direction)
  }

  object DiagonalDirection extends Enumeration {
    type DiagonalDirection = Value
    val UpRight, DownRight, DownLeft, UpLeft = Value
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

    /**
      * Sets a cell in the board.
      */
    def setCell(cell: BoardCell)

    /**
      * Transforms a board in String.
      */
    def toString: String

    /**
      * Creates a Map of orthogonal directions from a given coordinate.
      */
    def orthogonalCells(coordinate: Coordinate): Map[OrthogonalDirection, List[BoardCell]]

    /**
      * Creates a Map of diagonal directions from a given coordinate.
      */
    def diagonalCells(coordinate: Coordinate): Map[DiagonalDirection, List[BoardCell]]

    /**
      * Creates a list of special coordinates.
      */
    def specialCoordinates: List[Coordinate]

    /**
      * Creates a list of corner coordinates.
      */
    def cornerCoordinates: List[Coordinate]

    /**
      * Creates a coordinate of throne cell.
      */
    def centerCoordinates: Coordinate

    /**
      * Creates a copy of current board.
      */
    def getCopy: Board

    /**
      * Display a console representation of current board.
      */
    def consoleRepresentation: String

  }

  object Board {

    def apply(cells: Seq[Seq[BoardCell]]): Board = BoardImpl(cells)

    case class BoardImpl(private var allCells: Seq[Seq[BoardCell]]) extends Board {

      override def rows: Seq[Seq[BoardCell]] = allCells

      override def size: Int = allCells.length

      override def getCell(coordinate: Coordinate): BoardCell = allCells(coordinate.x - 1)(coordinate.y - 1)

      override def setCell(cell: BoardCell): Unit =
        allCells = allCells.map(_.map(c => if (c.getCoordinate.equals(cell.getCoordinate)) cell else c))

      override def equals(obj: Any): Boolean = this.rows.equals(obj.asInstanceOf[Board].rows)

      override def toString: String = rows.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")

      override def orthogonalCells(coordinate: Coordinate): Map[OrthogonalDirection, List[BoardCell]] =
        HashMap(OrthogonalDirection.Up -> upCells(coordinate),
          OrthogonalDirection.Right -> rightCells(coordinate),
          OrthogonalDirection.Down -> downCells(coordinate),
          OrthogonalDirection.Left -> leftCells(coordinate))

      override def diagonalCells(coordinate: Coordinate): Map[DiagonalDirection, List[BoardCell]] =
        HashMap(DiagonalDirection.UpRight -> upRightDiagonal(coordinate),
          DiagonalDirection.UpLeft -> upLeftDiagonal(coordinate),
          DiagonalDirection.DownRight -> downRightDiagonal(coordinate),
          DiagonalDirection.DownLeft -> downLeftDiagonal(coordinate))

      private def upCells(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x - 1 to 1 by -1).toList.map(Coordinate(_, coordinate.y)).map(getCell)

      private def rightCells(coordinate: Coordinate): List[BoardCell] =
        (coordinate.y + 1 to size).toList.map(Coordinate(coordinate.x, _)).map(getCell)

      private def downCells(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x + 1 to size).toList.map(Coordinate(_, coordinate.y)).map(getCell)

      private def leftCells(coordinate: Coordinate): List[BoardCell] =
        (coordinate.y - 1 to 1 by -1).toList.map(Coordinate(coordinate.x, _)).map(getCell)

      private def upRightDiagonal(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x - 1 to 1 by -1).toList.map(x => Coordinate(x, coordinate.x + coordinate.y - x)).filter(isInBoard).map(getCell)

      private def upLeftDiagonal(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x - 1 to 1 by -1).toList.map(x => Coordinate(x, coordinate.y - coordinate.x + x)).filter(isInBoard).map(getCell)

      private def downRightDiagonal(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x + 1 to size).toList.map(x => Coordinate(x, coordinate.y + x - coordinate.x)).filter(isInBoard).map(getCell)

      private def downLeftDiagonal(coordinate: Coordinate): List[BoardCell] =
        (coordinate.x + 1 to size).toList.map(x => Coordinate(x, coordinate.x - x + coordinate.y)).filter(isInBoard).map(getCell)

      private def isInBoard(coordinate: Coordinate): Boolean =
        coordinate.x >= 1 && coordinate.x <= size & coordinate.y >= 1 && coordinate.y <= size

      override def specialCoordinates: List[Coordinate] =
        cornerCoordinates :+ centerCoordinates

      override def cornerCoordinates: List[Coordinate] =
        List(Coordinate(1, 1), Coordinate(1, size), Coordinate(size, 1),
          Coordinate(size, size))

      override def centerCoordinates: Coordinate = Coordinate(size / 2 + 1, size / 2 + 1)

      override def getCopy: Board = BoardImpl(allCells)

      override def consoleRepresentation: String =
        rows.map(_.map(_.getPiece.toString).flatMap(e => if (e == Piece.Empty.toString) "_" else e)
          .mkString("  ") + "\n").mkString
    }

  }

}