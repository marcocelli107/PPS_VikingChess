package model.prolog

import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory}
import model.game.Player.Player
import model.game.BoardGame.Board.BoardImpl
import model.game.BoardGame.{Board, BoardCell}
import model.game.{Coordinate, Move, Piece, Player}

sealed trait ParserPrologTrait {

  /**
   * Creates a new Game.
   *
   * @return game as (PlayerToMove, Winner, Board, PiecesCapturedInTurn).
   */
  def createGame(newVariant: String): (Player, Player, Board, Int)

  /**
   * Gives possible moves from the selected cell.
   *
   * @param cell
   *            coordinate of the Cell.
   * @return seq of coordinates.
   */
  def showPossibleCells(cell: Coordinate): Seq[Coordinate]

  /**
   * Sets player move if it's legit.
   *
   * @param move
   *            move to make
   * @return new board.
   */
  def makeLegitMove(move: Move): (Player, Player, Board, Int)

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
   *                  coordinate of the cell to inspect
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                  coordinate of the cell to inspect
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a init pawn cell.
   *
   * @param coordinate
   *                  coordinate of the cell to inspect
   * @return boolean.
   */
  def isPawnCell(coordinate: Coordinate): Boolean

  /**
   * Get the Board
   *
   * @return the current board state.
   */
  def getCurrentBoard: Board

  /**
   * Player turn to move
   *
   * @return Player.
   */
  def getPlayer: Player

  /**
   * Undo some player move.
   *
   * @param oldBoard
   *                Board before changes
   * @return Unit.
   */
  def undoMove(oldBoard: Board): Unit
}

object ParserProlog extends ParserPrologTrait {

  import ImplicitParser._

  val THEORY: String = "src/main/scala/model/prolog/gameRules.pl"
  private val engine: Prolog = new Prolog()
  private var goal: SolveInfo = _
  private var list: Term = new Struct()
  private var winner: Term = new Struct()
  private var playerToMove: Term = new Struct()
  private var board: Term = new Struct()
  private var variant: Term = new Struct()

  private object Predicate extends Enumeration {
    type Predicate = Value
    val NewGame: Value = Value("newGame")
    val GetCoordPossibleMoves: Value = Value("getCoordPossibleMoves")
    val MakeLegitMove: Value = Value("makeLegitMove")
    val FindKing: Value = Value("findKing")
    val BoardSize: Value = Value("boardSize")
    val CornerCellCoord: Value = Value("cornerCellCoord")
    val CentralCellCoord: Value = Value("centralCellCoord")
    val IsInitialPawnCoord: Value = Value("isInitialPawnCoord")
    val UndoMove: Value = Value("undoMove")
  }
  engine.setTheory(new Theory(new FileInputStream(ParserProlog.THEORY)))

  override def getCurrentBoard: Board = board.parseBoard(getBoardSize)

  override def getPlayer: Player = playerToMove.parsePlayer

  override def createGame(newVariant: String): (Player, Player, Board, Int) = {
    goal = engine.solve(s"${Predicate.NewGame}($newVariant,(V,P,W,B)).")
    setGameTerms(goal)
    (goal.getTerm("P").parsePlayer, goal.getTerm("W").parsePlayer,
      board.parseBoard(getBoardSize), 0)
  }

  override def showPossibleCells(cell: Coordinate): Seq[Coordinate] = {
    goal = engine.solve(s"${Predicate.GetCoordPossibleMoves}(($variant,$playerToMove,$winner,$board), ${cell.toString}, L).")
    list = goal.getTerm("L")
    list.parseMoveList
  }

  override def makeLegitMove(move: Move): (Player, Player, Board, Int) = {
    goal = engine.solve(s"${Predicate.MakeLegitMove}(($variant, $playerToMove, $winner, $board)," +
      s"${move.from.toString}," +
      s"${move.to.toString}, L, (V,P,W,B)).")

    if(goal.isSuccess)
      setGameTerms(goal)

    (playerToMove.parsePlayer, winner.parsePlayer, board.parseBoard(getBoardSize), 0)
  }

  override def findKing(): Coordinate =
    engine.solve(s"${Predicate.FindKing}($board, Coord).").getTerm("Coord").parseMoveList.head

  override def isCentralCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.BoardSize}($variant,S), ${Predicate.CentralCellCoord}(S, ${coordinate.toString}).").isSuccess

  override def isCornerCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.BoardSize}($variant,S), ${Predicate.CornerCellCoord}(S, ${coordinate.toString}).").isSuccess

  override def isPawnCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.IsInitialPawnCoord}($variant, ${coordinate.toString}).").isSuccess

  override def undoMove(oldBoard: Board): Unit =
    setGameTerms(engine.solve(s"${Predicate.UndoMove}(($variant,$playerToMove,$winner,$board), $oldBoard, (V, P, W, B))."))

  private def setGameTerms(goal: SolveInfo): Unit = {
    variant = goal.getTerm("V")
    playerToMove = goal.getTerm("P")
    winner = goal.getTerm("W")
    board = goal.getTerm("B")
  }

  private def getBoardSize: Int = engine.solve(s"${Predicate.BoardSize}($variant,S).").getTerm("S").toString.toInt

  object ImplicitParser {

    private val prologListUselessChars: List[String] = List("','", "[", "]", "),", "(", ")")
    private val prologBoardUselessChars: List[String] = List("[", "]", "(", ")")

    //TODO commentare?
    implicit class StringUtils(base: String) {
      implicit def clearChars(chars: List[String]): String = base.filter(e => !chars.contains(e.toString))
      implicit def dropRightComma: String = if (base.last.equals(',')) base.dropRight(1) else base
    }

    implicit class BoardParser(base: Term) {
      implicit def parseBoard(size: Int): Board =
        BoardImpl(base.toString.clearChars(prologBoardUselessChars)
          .split(BoardCell.CELL_STRING).tail.map(_.parseCell).grouped(size).toSeq.map(_.toSeq))
    }

    implicit class CellParser(base: String) {
      implicit def parseCell: BoardCell = {
        val cell = base.dropRightComma
        BoardCell(cell.parseCoordinate, Piece.withName(cell.last.toString))
      }
    }

    private def arrayToCoordinate(a: Array[String]): Coordinate = Coordinate(a(0).toInt, a(1).toInt)

    implicit class CoordinateParser(base: String) {
      implicit def parseCoordinate: Coordinate = {
        val coordinate = base.replace(Coordinate.COORD_STRING, "").substring(0, base.length - 1).split(",")
        arrayToCoordinate(coordinate)
      }
    }

    implicit class MovesParser(base: Term) {
      implicit def parseMoveList: Seq[Coordinate] =
        base.toString.clearChars(prologListUselessChars).split(Coordinate.COORD_STRING).tail
          .map(e => arrayToCoordinate(e.split(",")))
    }

    implicit class PlayerParser(base: Term) {
      implicit def parsePlayer: Player = Player.values.filter(_.parserString.equals(base.toString)).head
    }
  }
}
