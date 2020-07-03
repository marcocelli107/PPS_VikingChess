package actor_ia

import akka.actor.{Actor, ActorRef, Props}
import ia.EvaluationFunction
import model.{GameSnapshot, ModelHnefatafl}
import utils.BoardGame.Board
import utils.Move

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParHashMap

case class MadeMoveMsg(gameSnapshot: GameSnapshot)

case class EvaluatedMsg(score: Int)

case class AbdadaIA(model: ModelHnefatafl, depth: Int) extends Actor {

  override def receive: Receive = {
    case m: FindBestMoveMsg => findBestMove(m.gameSnapshot)
    case m: ReturnBestMoveMsg => model.makeMove(m.bestMove); context.stop(sender())
  }

  def working(father: ActorRef, gameSnapshot: GameSnapshot): Receive = {
    case m: RetrieveAnswer => answerRetrieved(father, gameSnapshot, m.alpha, m.beta, m.score)
    case m: MadeMoveMsg => context.become(working(father, m.gameSnapshot))
   // case m: EvaluatedMsg
  }

  private def findBestMove(gameSnapshot: GameSnapshot): Unit = ???

  private def abdada(father: ActorRef, gameSnapshot: GameSnapshot, move: Option[Move], alpha: Int, beta: Int, depth: Int): Unit = {
    if(depth == 0)
      evaluatePosition(gameSnapshot)
    else {
      // TODO IL WRITER DEVE ESSERE UNICO
      val ttWriter: ActorRef = context.actorOf(Props(TranspositionTableWriter()))
      // TODO LA TT DEVE ESSERE UNICA
      val tt: TranspositionTable = TranspositionTable(ParHashMap.empty)
      ttWriter ! InitTTWMsg(tt)

      context.actorOf(Props(TranspositionTableRetriever())) !
        RetrieveAsk(ttWriter, tt, gameSnapshot.getBoard, alpha, beta, depth)
      context.become(working(father, gameSnapshot))
      makeMove(gameSnapshot, move)
    }
  }

  def evaluatePosition(gameSnapshot: GameSnapshot): Unit = {
    self ! EvaluatedMsg(EvaluationFunction.score(gameSnapshot))
  }

  private def makeMove(fatherSnapshot: GameSnapshot, move: Option[Move]): Unit = {
    self ! MadeMoveMsg(newMoveSnapshot(fatherSnapshot, move))
  }

  private def newMoveSnapshot(fatherSnapshot: GameSnapshot, move: Option[Move]): GameSnapshot =
    if (move.nonEmpty)
      MoveGenerator.makeMove(fatherSnapshot, move.get)
    else
      fatherSnapshot

  private def answerRetrieved(father: ActorRef, gameSnapshot: GameSnapshot, alpha: Int, beta: Int, best: Int): Unit = {
    //var best = Int.MinValue
    if(alpha >= beta || best == TranspositionTableRetriever.ON_EVALUATION)
      father ! EvaluatedMsg(EvaluationFunction.score(gameSnapshot))
    else {
      var iteration: Int = 0
      var allDone: Boolean = false
      var possibleMoves: ListBuffer[Move] = MoveGenerator.gamePossibleMoves(gameSnapshot).to[ListBuffer]
      while(iteration < 2 && alpha < beta || !allDone) {
        iteration += 1
        allDone = true
        var move: Option[Move] = Option.empty
        if(possibleMoves.nonEmpty) {
          move = Option(possibleMoves.head)
          possibleMoves = possibleMoves.drop(1)
        }
        while(move.isDefined && alpha < beta) {
          // TODO exclusive??
          //val value =
        }

      }

    }
  }


}

