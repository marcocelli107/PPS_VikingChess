package model

import utils.BoardGame.Board
import utils.BoardGame.BoardCell
import utils.Pair
import scala.collection.mutable.ListBuffer


class MiniMax(parser: ParserProlog, currentBoard: Board, player: Player.Value) {

  private var board = currentBoard
  private var iAPlayer = player
  private var parserProlog = parser



  def createBoardHistory(board: Board): List[Board] = {
    var boardHistory: List[Board] = List()
    var cellBufferList: ListBuffer[Pair[Int]] = null
    var boardCopy: Board = null

    boardHistory ::= board

    /*board.cells.forEach(c => {
      cellBufferList = createBufferedList(c.getCoordinate)
      board.getCell(c.getCoordinate).setPiece(PieceEnum.Void)
      boardCopy = board
      cellBufferList.foreach(coord => {
        boardCopy.getCell(coord).setPiece(PieceEnum.BlackPawn)
        boardHistory ::= boardCopy
        boardCopy = board
      })
    })*/

    println(boardHistory.size)
    println(boardHistory)

    boardHistory
  }

  def createBufferedList(coordinate: Pair[Int]): ListBuffer[Pair[Int]] = {
    var cellBufferList: ListBuffer[Pair[Int]] = null

    cellBufferList = parserProlog.showPossibleCells(coordinate)

    cellBufferList
  }


}

object TryMinMax extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserProlog(THEORY)
  val board = parserProlog.createGame(GameVariant.Hnefatafl.nameVariant)._3
  val miniMax = new MiniMax(parserProlog, board, Player.Black)

  miniMax.createBoardHistory(board)

}


