package model

import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory}
import utils.BoardGame.Board.BoardImpl
import utils.BoardGame.{Board, BoardCell}
import utils.Coordinate

import scala.collection.mutable.ListBuffer

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
  def showPossibleCells(cell: Coordinate): ListBuffer[Coordinate]

  /**
    * Sets player move if it's legit.
    * @param cellStart
    *                 coordinate of the starting cell.
    * @param cellArrival
    *                 coordinate of the arrival cell.
    * @return new board.
    */
  def makeMove(cellStart: Coordinate, cellArrival: Coordinate): (Player.Value, Player.Value, Board, Int)

  /**
   * Undoes the last move.
   * @param fromCoordinate
   *                 coordinate of the starting cell.
   * @param toCoordinate
   *                 coordinate of the arrival cell.
   * @return new board.
   */
  def undoMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): (Player.Value, Player.Value, Board, Int)

  /**
    * Finds king on game board.
    *
    * @return king's coordinate.
    */
  def findKing(): Coordinate

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
    * Checks if the cell at the specified coordinate is a init pawn cell.
    *
    * @param coordinate
    *                   coordinate of the cell to inspect
    *
    * @return boolean.
    */
  def isPawnCell(coordinate: Coordinate): Boolean

  /**
   * Copy himself.
   *
   * @return ParserProlog.
   */

  def copy():ParserProlog

  /**
   * Return the board
   *
   * @return Board.
   */

  def getActualBoard:Board

  /**
   * Returns the current player
   *
   * @return Board.
   */

  def getPlayer: Player.Value
}

object ParserPrologImpl {

  def apply(theory: String): ParserProlog = new ParserPrologImpl(theory)

  def apply(theory: String,_playerToWin: Term ,_playerToMove: Term , _board: Term, _variant: Term): ParserPrologImpl = {
    val ppi=new ParserPrologImpl(theory)
    ppi.playerToWin= _playerToWin
    ppi.playerToMove= _playerToMove
    ppi.board= _board
    ppi.variant= _variant
    ppi
  }

}

case class ParserPrologImpl(theory: String) extends ParserProlog {

  private var goalString: String = ""

  private val engine = new Prolog()
  private var goal: SolveInfo = _
  private var list: Term = new Struct()
  private var playerToWin: Term = new Struct()
  private var playerToMove: Term = new Struct()
  private var board: Term = new Struct()
  private var variant: Term = new Struct()

  def getPlayerToWin:Term = playerToWin

  def getPlayerToMove:Term =  playerToMove

  def getBoard: Term = board

  def getVariant: Term = variant

  engine.setTheory(new Theory(new FileInputStream(theory)))

  override def createGame(newVariant: String): (Player.Value, Player.Value, Board, Int) = {
    goal = engine.solve(s"newGame($newVariant,(V,P,W,B)).")

    setGameTerms(goal)

    goalString = replaceBoardString(board)

    (setPlayer(goal.getTerm("P").toString), setPlayer(goal.getTerm("W").toString), parseBoard(goalString), 0)
  }

  override def showPossibleCells(cell: Coordinate): ListBuffer[Coordinate] = {

    goal = engine.solve(s"getCoordPossibleMoves(($variant,$playerToMove,$playerToWin,$board), coord(${cell.x}, ${cell.y}), L).")

    list = goal.getTerm("L")

    goalString = replaceListCellsString(list)

    setListCellsView(goalString)
  }

  override def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): (Player.Value, Player.Value, Board, Int) =
    setMove(fromCoordinate, toCoordinate, "makeLegitMove")

  override def undoMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): (Player.Value, Player.Value, Board, Int) =
    setMove(fromCoordinate, toCoordinate, "makeMove")

  private def setMove(fromCoordinate: Coordinate, toCoordinate: Coordinate, predicate: String): (Player.Value, Player.Value, Board, Int) = {
    goal = engine.solve(s"$predicate(($variant, $playerToMove, $playerToWin, $board)," +
      s"coord(${fromCoordinate.x},${fromCoordinate.y})," +
      s"coord(${toCoordinate.x},${toCoordinate.y}), L, (V,P,W,B)).")

    setGameTerms(goal)

    goalString = replaceBoardString(board)

    (setPlayer(goal.getTerm("P").toString), setPlayer(goal.getTerm("W").toString), parseBoard(goalString), goal.getTerm("L").toString.toInt)
  }

  override def findKing(): Coordinate = {
    goal = engine.solve(s"findKing($board, Coord).")
    list = goal.getTerm("Coord")

    goalString = replaceListCellsString(list)

    setListCellsView(goalString).head
  }

  override def isCentralCell(coordinate: Coordinate): Boolean = {
    goal = engine.solve(s"boardSize($variant,S), centralCellCoord(S, coord(${coordinate.x},${coordinate.y})).")

    goal.isSuccess
  }

  override def isCornerCell(coordinate: Coordinate): Boolean = {
    goal = engine.solve(s"boardSize($variant,S), cornerCellCoord(S, coord(${coordinate.x},${coordinate.y})).")

    goal.isSuccess
  }

  override def isPawnCell(coordinate: Coordinate): Boolean = {
    goal = engine.solve(s"isInitialPawnCoord($variant, coord(${coordinate.x},${coordinate.y})).")

    goal.isSuccess
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
  private def parseBoard(stringBoard: String): Board = {

    var listCells: ListBuffer[BoardCell] = ListBuffer.empty[BoardCell]

    stringBoard.split("cell").tail.foreach(elem => {
      var coordinateCell: Array[String] = null
      if (elem.contains("e")) {
        coordinateCell = elem.substring(0, elem.length - 2).split(",")
        listCells += BoardCell(Coordinate(coordinateCell(0).toInt, coordinateCell(1).toInt), Piece.Void)
      } else if (elem.contains("bp")) {
        coordinateCell = elem.substring(0, elem.length - 3).split(",")
        listCells += BoardCell(Coordinate(coordinateCell(0).toInt, coordinateCell(1).toInt), Piece.BlackPawn)
      } else if (elem.contains("wp")) {
        coordinateCell = elem.substring(0, elem.length - 3).split(",")
        listCells += BoardCell(Coordinate(coordinateCell(0).toInt, coordinateCell(1).toInt), Piece.WhitePawn)
      } else if (elem.contains("wk")) {
        coordinateCell = elem.substring(0, elem.length - 3).split(",")
        listCells += BoardCell(Coordinate(coordinateCell(0).toInt, coordinateCell(1).toInt), Piece.WhiteKing)
      }
    })

    BoardImpl(listCells)
  }

  /**
   * Parses the pieces captured list: from prolog to scala style.
   *
   * @param stringList
   *              list identification string.
   *
   * @return parsed list.
   */
  private def setListCellsView(stringList: String): ListBuffer[Coordinate] = {
    var listPossibleCoordinates: ListBuffer[Coordinate] = ListBuffer.empty[Coordinate]

    goalString.split("coord").tail.foreach(elem => {
      var coordinateCells: Array[String] = null
      coordinateCells = elem.split(",")
      listPossibleCoordinates += Coordinate(coordinateCells(0).toInt, coordinateCells(1).toInt)
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

  /**
   * Copy himself.
   *
   * @return ParserProlog.
   */
  override def copy(): ParserProlog = ParserPrologImpl(theory, playerToWin, playerToMove, board, variant)

  /**
   * Return the board
   *
   * @return Board.
   */

  override def getActualBoard: Board = {
    goalString = replaceBoardString(board)
    parseBoard(goalString)
  }

  /**
   * Returns the current player
   *
   * @return Board.
   */

  override def getPlayer: Player.Value = {
    setPlayer(playerToMove.toString)
  }

  override def equals(that: Any): Boolean = that match {
    case that: ParserPrologImpl =>
      that.isInstanceOf[ParserPrologImpl] &&
        that.variant.isEqualObject(this.variant) &&
        that.playerToMove.isEqualObject(this.playerToMove) &&
        that.playerToWin.isEqualObject(this.playerToWin) &&
        that.board.isEqualObject(this.board)

    case _ => false

  }

}
