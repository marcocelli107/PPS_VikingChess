package ia.minimax

import akka.actor.Props
import model.game.BoardGame.Board
import model.game.Level.Level
import model.game.Move

import scala.collection.mutable

class MaxActor(levelIA: Level) extends MiniMaxActor(levelIA) {

  override def initAlfa: Int = Int.MinValue

  override def createChild(): Props =
    Props(new MinActor(levelIA))

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    score > alfa
  }

  def printScores(board: Board, scores: mutable.Map[Move, Int], score: Int, move: Option[Move]): Unit =
    print("move: " + move.getOrElse("none") + "\n" +
      board.consoleRepresentation + "\n" +
      "scores :" + scores + "\n" +
      "expected: " + scores.values.max + "\n" +
      "real: " + score + "\n\n"
    )
}
