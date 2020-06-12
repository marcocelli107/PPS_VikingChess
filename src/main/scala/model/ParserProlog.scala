package model

import java.io.FileInputStream
import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory}
import utils.Board.Board.BoardImpl
import utils.Board.BoardCell.BoardCellImpl
import utils.Board.{Board, BoardCell}
import utils.Pair
import utils.Pair.PairImpl

import scala.collection.mutable.ListBuffer

trait ParserProlog {

  /**
    * Creates a new Game.
    *
    * @return game as (PlayerToMove, Winner, Board, PiecesCapturedInTurn).
    */
  def createGame(newVariant: String): (Player.Value, Player.Value, Board, Int)

  /**
    * Gives possible moves from the selected cell
    * @param cell
    *                 coordinate of the Cell.
    * @return list buffer of coordinates.
    */
  def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]]

  /**
    * Sets player move if it's legit
    * @param cellStart
    *                 coordinate of the starting cell.
    * @param cellArrival
    *                 coordinate of the arrival cell.
    * @return new board.
    */
  def makeMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Player.Value, Player.Value, Board, Int)
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
      goal = engine.solve("retractall(playerToMove(_)), retractall(winner(_)), retractall(board(_))," +
        s"newGame($newVariant,(V,P,W,B))," +
        "assert(variant(V)),assert(playerToMove(P)),assert(winner(W)),assert(board(B)).")

      board = goal.getTerm("B")
      variant = goal.getTerm("V")
      playerToMove = goal.getTerm("P")
      playerToWin = goal.getTerm("W")

      goalString = replaceBoardString(goal.getTerm("B"))

      setModelBoard(goalString)

      (setPlayer(goal.getTerm("P").toString), setPlayer(goal.getTerm("W").toString), myBoard, 0)
    }

    override def showPossibleCells(cell: Pair[Int]): ListBuffer[Pair[Int]] = {

      /*goal = engine.solve(s"variant(V), playerToMove(P), winner(W), board(B)," +
        s"getCoordPossibleMoves((V,P,W,B), coord(${cell.getX}, ${cell.getY}), L).")*/

      goal = engine.solve(s"variant(V), playerToMove(P), winner(W), board(B)," +
        s"getCoordPossibleMoves(($variant,$playerToMove,$playerToWin,$board), coord(${cell.getX}, ${cell.getY}), L).")
      list = goal.getTerm("L")

      goalString = replaceListCellsString(list)

      setListCellsView(goalString)
    }

    override def makeMove(cellStart: Pair[Int], cellArrival: Pair[Int]): (Player.Value, Player.Value, Board, Int) = {
      /*goal = engine.solve(s"variant(V), playerToMove(P), winner(W), board(B), " +
        "makeLegitMove((V, P, W, B),coord(${cellStart.getX},${cellStart.getY}),coord(${cellArrival.getX},${cellArrival.getY}), L, (V2,P2,W2,B2))," +
        "assert(variant(V2)), assert(playerToMove(P2)),assert(winner(W2)),assert(board(B2)).")*/

      goal = engine.solve(s"variant(V), playerToMove(P), winner(W), board(B), " +
        s"makeLegitMove(($variant, $playerToMove, $playerToWin, $board),coord(${cellStart.getX},${cellStart.getY}),coord(${cellArrival.getX},${cellArrival.getY}), L, (V2,P2,W2,B2))," +
        "assert(variant(V2)), assert(playerToMove(P2)),assert(winner(W2)),assert(board(B2)).")

      goalString = replaceBoardString(goal.getTerm("B2"))

      board = goal.getTerm("B2")
      variant = goal.getTerm("V2")
      playerToMove = goal.getTerm("P2")
      playerToWin = goal.getTerm("W2")

      setModelBoard(goalString)

      (setPlayer(goal.getTerm("P2").toString), setPlayer(goal.getTerm("W2").toString), myBoard, goal.getTerm("L").toString.toInt)
    }

    private def replaceBoardString(board: Term): String = board.toString.replace("[", "").replace("]", "")
      .replace("(", "").replace(")", "").replace("coord", "")

    private def replaceListCellsString(list: Term): String = list.toString.replace("','", "").replace("[", "").replace("]", "")
      .replace("),", "").replace("(", "").replace(")", "")

    private def setModelBoard(stringBoard: String): Unit = {

      var listCells: ListBuffer[BoardCell] = ListBuffer.empty[BoardCell]

      stringBoard.split("cell").tail.foreach(elem => {
        var coordinateCell: Array[String] = null
        if (elem.contains("e")) {
          coordinateCell = elem.substring(0, elem.length - 2).split(",")
          listCells += BoardCellImpl(PairImpl(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.Void)
        } else if (elem.contains("bp")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCellImpl(PairImpl(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.BlackPawn)
        } else if (elem.contains("wp")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCellImpl(PairImpl(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.WhitePawn)
        } else if (elem.contains("wk")) {
          coordinateCell = elem.substring(0, elem.length - 3).split(",")
          listCells += BoardCellImpl(PairImpl(coordinateCell(0).toInt, coordinateCell(1).toInt), PieceEnum.WhiteKing)
        }
      })

      myBoard = BoardImpl(listCells)
    }

    private def setListCellsView(stringList: String): ListBuffer[Pair[Int]] = {
      var listPossibleCoordinates: ListBuffer[Pair[Int]] = ListBuffer.empty[Pair[Int]]

      goalString.split("coord").tail.foreach(elem => {
        var coordinateCells: Array[String] = null
        coordinateCells = elem.split(",")
        listPossibleCoordinates += PairImpl(coordinateCells(0).toInt, coordinateCells(1).toInt)
      })
      listPossibleCoordinates
    }

    private def setPlayer(stringPlayer: String): Player.Value = stringPlayer match {
      case "w" => Player.White
      case "b" => Player.Black
      case "d" => Player.Draw
      case _ => Player.None
    }
  }
}
