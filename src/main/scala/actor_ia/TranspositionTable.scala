package actor_ia

import actor_ia.Flag.Flag
import akka.actor.{Actor, ActorRef, PoisonPill}
import utils.BoardGame.Board

import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.ParHashMap

object Flag extends Enumeration {
  type Flag = Value
  val LBOUND, UBOUND, VALID, UNSET = Value
}

case class TableValue(score: Int, depth: Int, flag: Flag, nProc: Int)

case class TranspositionTable(transpositionTable: mutable.ParHashMap[Board, TableValue]) {
  def read(board: Board): Option[TableValue] = Option(transpositionTable.getOrElse(board, null))
  def write(board: Board, tableValue: TableValue): Unit = transpositionTable += (board -> tableValue)
}

case class RetrieveAsk(ttWriter: ActorRef, transpositionTable: TranspositionTable, board: Board, alpha: Int, beta: Int, depth: Int)

case class UpdateTableMsg(board: Board, tableValue: TableValue)

case class RetrieveAnswer(alpha: Int, beta: Int, score: Int)

object TranspositionTableRetriever {
  val ON_EVALUATION: Int = 9999
}

case class TranspositionTableRetriever() extends Actor {

  override def receive: Receive = {
    case m: RetrieveAsk => retrieveAsk(sender(), m.ttWriter, m.transpositionTable, m.board, m.alpha, m.beta, m.depth)
  }

  private def retrieveAsk(sender: ActorRef, ttWriter: ActorRef, tT: => TranspositionTable, board: Board, alpha: Int, beta: Int, depth: Int): Unit = {
    val entry: Option[TableValue] = tT.read(board)
    var answerAlpha = alpha
    var answerBeta = beta
    var answerScore = Int.MinValue
    if(entry.isEmpty) {
    } else if(entry.get.depth == depth && entry.get.nProc > 0) {
      answerScore = TranspositionTableRetriever.ON_EVALUATION
    } else if(entry.get.depth >= depth) {
      if(entry.get.flag.equals(Flag.VALID)) {
        answerScore = entry.get.score
        answerBeta = entry.get.score
      } else if(entry.get.flag.equals(Flag.UBOUND) && entry.get.score < beta) {
        answerScore = entry.get.score
        answerAlpha = entry.get.score
      } else if(entry.get.flag.equals(Flag.LBOUND) && entry.get.score > alpha) {
        answerScore = entry.get.score
        answerAlpha = entry.get.score
      }
      if(entry.get.depth == depth && answerAlpha < answerBeta) {
        ttWriter ! UpdateTableMsg(board, TableValue(entry.get.score, entry.get.depth, entry.get.flag, entry.get.nProc + 1))
      }
    } else {
      ttWriter ! UpdateTableMsg(board, TableValue(entry.get.score, depth, Flag.UNSET, 1))
    }
    sender ! RetrieveAnswer(answerAlpha, answerBeta, answerScore)
    self ! PoisonPill
  }

}

case class InitTTWMsg(transpositionTable: TranspositionTable)

case class StoreHashMsg(board: Board, alpha: Int, beta: Int, score: Int, depth: Int)

case class TranspositionTableWriter() extends Actor {
  override def receive: Receive = {
    case m: InitTTWMsg => context.become(workingState(m.transpositionTable))
  }

  def workingState(tT: => TranspositionTable): Receive = {
    case m: UpdateTableMsg => tT.write(m.board, m.tableValue)
    case m: StoreHashMsg => storeHash(tT, m.board, m.alpha, m.beta, m.score, m.depth)
  }

  def storeHash(tT: => TranspositionTable, board: Board, alpha: Int, beta: Int, score: Int, depth: Int): Unit = {
    val entry: Option[TableValue] = tT.read(board)
    var nProc = 0
    var flag = Flag.VALID
    if(entry.isEmpty || entry.get.depth > depth)
      return
    if(entry.get.depth == depth)
      nProc = entry.get.nProc - 1
    if(score >= beta)
      flag = Flag.LBOUND
    else if(score <= alpha)
      flag = Flag.UBOUND
    tT.write(board, TableValue(score, depth, flag, nProc))
  }

}

