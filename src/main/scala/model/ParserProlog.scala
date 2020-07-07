package model

import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory}
import model.Player.Player
import utils.BoardGame.Board.BoardImpl
import utils.BoardGame.{Board, BoardCell}
import utils.{Coordinate, Move}

trait ParserProlog {

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
   * coordinate of the Cell.
   * @return seq of coordinates.
   */
  def showPossibleCells(cell: Coordinate): Seq[Coordinate]

  /**
   * Sets player move if it's legit.
   *
   * @param move
   * move to make
   * @return new board.
   */
  def makeLegitMove(move: Move): (Player, Player, Board, Int)
/* TODO NON LA USIAMO PIU?
  /**
   * Undoes the last move.
   *
   * @param move
   * move to undo
   * @return new board.
   */
  def makeNonLegitMove(move: Move): (Player.Val, Player.Val, Board, Int)
*/
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
   * coordinate of the cell to inspect
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   * coordinate of the cell to inspect
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a init pawn cell.
   *
   * @param coordinate
   * coordinate of the cell to inspect
   * @return boolean.
   */
  def isPawnCell(coordinate: Coordinate): Boolean

  /**
   * Copy himself.
   *
   * @return parser representing the game state.
   */
  def copy(): ParserProlog

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
   * Board before changes
   * @return Unit.
   */
  def undoMove(oldBoard: Board): Unit
/* TODO NON LO USIAMO PIU?
  /**
   * Takes all the possible moves of a player from a game state
   *
   * @return List of coordinates tuples
   */
  def gamePossibleMoves(): List[(Coordinate, Coordinate)]
*/
}

object ParserPrologImpl {

  val THEORY: String = "src/main/scala/model/gameRules.pl"

  def apply(): ParserProlog = new ParserPrologImpl()

  // TODO eliminare se non serve più copy
  def apply(_playerToWin: Term, _playerToMove: Term, _board: Term, _variant: Term): ParserPrologImpl = {
    val ppi = new ParserPrologImpl()
    ppi.winner = _playerToWin
    ppi.playerToMove = _playerToMove
    ppi.board = _board
    ppi.variant = _variant
    ppi
  }

}

case class ParserPrologImpl() extends ParserProlog {

  import ImplicitParser._
  import Predicate.Predicate

  private val engine = new Prolog()
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
    val MakeMove: Value = Value("makeMove")
    val FindKing: Value = Value("findKing")
    val BoardSize: Value = Value("boardSize")
    val CornerCellCoord: Value = Value("cornerCellCoord")
    val CentralCellCoord: Value = Value("centralCellCoord")
    val IsInitialPawnCoord: Value = Value("isInitialPawnCoord")
    // TODO ELIMINARE SE NON USATO
    val GamePossibleMoves: Value = Value("gamePossibleMoves")
    val UndoMove: Value = Value("undoMove")
  }

  engine.setTheory(new Theory(new FileInputStream(ParserPrologImpl.THEORY)))

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

  override def makeLegitMove(move: Move): (Player, Player, Board, Int) = setMove(move, Predicate.MakeLegitMove)

  // TODO NON LA USIAMO PIU'?
  /* override def makeNonLegitMove(move: Move): (Player, Player, Board, Int) =
    setMove(move, "makeMove")
    */

  // TODO INGLOBARE IN MakeLegitMove SE NON USIAMO PIU MakeNonLegit
  private def setMove(move: Move, predicate: Predicate): (Player, Player, Board, Int) = {
    goal = engine.solve(s"$predicate(($variant, $playerToMove, $winner, $board)," +
      s"${move.from.toString}," +
      s"${move.to.toString}, L, (V,P,W,B)).")

    setGameTerms(goal)

    (goal.getTerm("P").parsePlayer, goal.getTerm("W").parsePlayer,
      board.parseBoard(getBoardSize), goal.getTerm("L").toString.toInt)
  }

  override def findKing(): Coordinate =
    engine.solve(s"${Predicate.FindKing}($board, Coord).").getTerm("Coord").parseMoveList.head

  override def isCentralCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.BoardSize}($variant,S), ${Predicate.CentralCellCoord}(S, ${coordinate.toString}).").isSuccess

  override def isCornerCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.BoardSize}($variant,S), ${Predicate.CornerCellCoord}(S, ${coordinate.toString}).").isSuccess

  override def isPawnCell(coordinate: Coordinate): Boolean =
    engine.solve(s"${Predicate.IsInitialPawnCoord}($variant, ${coordinate.toString}).").isSuccess

  // TODO SERVE ANCORA?
  override def copy(): ParserProlog = ParserPrologImpl(winner, playerToMove, board, variant)

  override def equals(that: Any): Boolean = that match {
    case that: ParserPrologImpl =>
      that.isInstanceOf[ParserPrologImpl] &&
        that.variant.isEqualObject(this.variant) &&
        that.playerToMove.isEqualObject(this.playerToMove) &&
        that.winner.isEqualObject(this.winner) &&
        that.board.isEqualObject(this.board)
    case _ => false
  }

  override def undoMove(oldBoard: Board): Unit =
    setGameTerms(engine.solve(s"${Predicate.UndoMove}(($variant,$playerToMove,$winner,$board), $oldBoard, (V, P, W, B))."))

  /* TODO NON LO USIAMO PIU?
  def gamePossibleMoves(): List[(Coordinate, Coordinate)] = {
    goal = engine.solve(s"gamePossibleMoves(($variant,$playerToMove,$winner,$board), L).")

    list = goal.getTerm("L")

    goalString = replaceListCellsString(list)

    setListCellsView(goalString).grouped(2).toList.map(listBuffer => (listBuffer.head, listBuffer(1)))
  }*/

  /**
   * Sets game's terms returned from prolog.
   */
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
