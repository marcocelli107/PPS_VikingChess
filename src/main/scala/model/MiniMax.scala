package model

import utils.BoardGame.{Board, BoardCell}
import utils.{BoardGame, Pair}

import scala.collection.mutable.ListBuffer


class MiniMax(parser: ParserProlog, player: Player.Value) {

  private var parserProlog = parser.copy

  def createParserHistory(): List[ParserProlog] = {
    var parserHistory: List[ParserProlog] = List()
    var cellBufferList: ListBuffer[Pair[Int]] = null
    var board: Board = parserProlog.getActualBoard()
    var parserCopy: ParserProlog = null

    parserHistory ::= parserProlog

    board.cells.foreach(c => {
      cellBufferList = createBufferedList(c.getCoordinate)
      cellBufferList.foreach(coord => {
        parserCopy = parserProlog.copy
        parserCopy.makeMove(c.getCoordinate, coord)
        parserHistory ::= parserCopy
      })
    })

    println(parserHistory.size)

    parserHistory.foreach(p => {
        println(p.getActualBoard())
    })

    parserHistory
  }

  def createBufferedList(coordinate: Pair[Int]): ListBuffer[Pair[Int]] = {
    var cellBufferList: ListBuffer[Pair[Int]] = null

    cellBufferList = parserProlog.showPossibleCells(coordinate)

    cellBufferList
  }
}

object TryMinMax extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  val miniMax = new MiniMax(parserProlog, Player.Black)

  miniMax.createParserHistory()

}



