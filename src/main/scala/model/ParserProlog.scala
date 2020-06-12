package model

import java.io.FileInputStream
import scala.collection.mutable.ListBuffer
import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory}
import utils.BoardGame.{Board, BoardCell}
import utils.Pair

trait ParserProlog {

  /**
    * Creates a new Game.
    *
    * @return game as (PlayerToMove, Winner, Board, PiecesCapturedInTurn).
    */
  def createGame(newVariant: String): (Player.Value, Player.Value, Board, Int)

  /**
    * Gives possible moves from the selected cell.
    * @param cell
    *                 coordinate of the Cell.
    * @return list buffer of coordinates.
    */
  def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]]

  /**
    * Sets player move if it's legit.
    * @param cellStart
    *                 coordinate of the starting cell.
    * @param cellArrival
    *                 coordinate of the arrival cell.
    * @return new board.
    */
  def makeMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Player.Value, Player.Value, Board, Int)

  /**
    * Finds king on game board.
    *
    * @return king's coordinate.
    */
  def findKing(): ListBuffer[Pair[Int]]
}

object ParserProlog {

  def apply(theory: String): ParserProlog = ParserPrologImpl(theory)

  case class ParserPrologImpl(theory: String) extends ParserProlog {

    private var goalString: String = ""

    private val engine = new Prolog()
    private var goal: SolveInfo = _
    private var list: Term = new Struct()
    private var myBoard: Board = _
    private var playerToWin: Term = new Struct()
    private var playerToMove: Term = new Struct()
    private var board: Term = new Struct()
    private var variant: Term = new Struct()


    engine.setTheory(new Theory(new FileInputStream(theory)))

    override def createGame(newVariant: String): (Player.Value, Player.Value, Board, Int) = {
      goal = engine.solve(s"newGame($newVariant,(V,P,W,B)).")

      setGameTerms(goal)

      goalString = replaceBoardString(board)

      setModelBoard(goalString)

      (setPlayer(goal.getTerm("P").toString), setPlayer(goal.getTerm("W").toString), myBoard, 0)
    }

    override def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]] = {

      goal = engine.solve(s"getCoordPossibleMoves(($variant,$playerToMove,$playerToWin,$board), coord(${cell.getX}, ${cell.getY}), L).")

      list = goal.getTerm("L")

      goalString = replaceListCellsString(list)

      setListCellsView(goalString)
    }

    override def makeMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Player.Value, Player.Value, Board, Int) = {

      goal = engine.solve(s"makeLegitMove(($variant, $playerToMove, $playerToWin, $board),coord(${cellStart.getX},${cellStart.getY}),coord(${cellArrival.getX},${cellArrival.getY}), L, (V,P,W,B)).")

      setGameTerms(goal)

      goalString = replaceBoardString(board)

      setModelBoard(goalString)

      (setPlayer(goal.getTerm("P").toString), setPlayer(goal.getTerm("W").toString), myBoard, goal.getTerm("L").toString.toInt)
    }

    override def findKing(): ListBuffer[Pair[Int]] = {
      goal = engine.solve(s"findKing($board, Coord).")
      list = goal.getTerm("Coord")

      goalString = replaceListCellsString(list)

      setListCellsView(goalString)
    }

    /**
      * Sets game's terms returned from prolog.
      */
    private def setGameTerms(goal: SolveInfo): Unit = {
      variant = goal.getTerm("V")
      playerToMove = goal.getTerm("P")
      playerToWin = goal.getTerm("W")
      board = goal.getTerm("B")
    }

    /**
      * Cleans the board returned in output.
      *
      * @param board
      *              board identification term.
      *
      * @return board to string.
      */
    private def replaceBoardString(board: Term): String = board.toString.replace("[", "").replace("]", "")
      .replace("(", "").replace(")", "").replace("coord", "")

    /**
      * Cleans the pieces captured list returned in output.
      *
      * @param list
      *              list identification term.
      *
      * @return list to string.
      */
    private def replaceListCellsString(list: Term): String = list.toString.replace("','", "").replace("[", "").replace("]", "")
      .replace("),", "").replace("(", "").replace(")", "")

    /**
      * Parses the board: from prolog to scala style.
      *
      * @param stringBoard
      *              board identification string.
      */
    private def setModelBoard(stringBoard: String): Unit = {

      var listCells: ListBuffer[BoardCell] = ListBuffer.empty[BoardCell]

      stringBoard.split("cell").tail.foreach(elem => {
        var coordinateCell: Array[String] = null
        if (elem.contains("e")) {
          coordinateCell = elem.substring(0, elem.length - 2).split(",")
          listCells += BoardCell(Pair(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.Void)
        } else if (elem.contains("bp")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCell(Pair(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.BlackPawn)
        } else if (elem.contains("wp")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCell(Pair(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.WhitePawn)
        } else if (elem.contains("wk")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCell(Pair(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.WhiteKing)
        }
      })

      myBoard = Board(listCells)
    }

    /**
      * Parses the pieces captured list: from prolog to scala style.
      *
      * @param stringList
      *              list identification string.
      *
      * @return parsed list.
      */
    private def setListCellsView(stringList: String): ListBuffer[Pair[Int]] = {
      var listPossibleCoordinates: ListBuffer[Pair[Int]] = ListBuffer.empty[Pair[Int]]

      goalString.split("coord").tail.foreach(elem => {
        var coordinateCells: Array[String] = null
        coordinateCells = elem.split(",")
        listPossibleCoordinates += Pair(coordinateCells(0).toInt, coordinateCells(1).toInt)
      })
      listPossibleCoordinates
    }

    /**
      * Parses the string player in a enum.
      *
      * @param stringPlayer
      *              player identification string.
      *
      * @return parsed player.
      */
    private def setPlayer(stringPlayer: String): Player.Value = stringPlayer match {
      case "w" => Player.White
      case "b" => Player.Black
      case "d" => Player.Draw
      case _ => Player.None
    }
  }
}
