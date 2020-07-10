package ia.minimax

import akka.actor.{Actor, ActorRef, Props}
import ia.evaluation_function.EvaluationFunction
import model.game.Level.Level
import model.game.Player.Player
import model.game.{GameSnapshot, Move, MoveGenerator, Player}

import scala.collection.{immutable, mutable}

/**
 * The InitMsg message tells the actors to start exploring the tree starting from a specific snapshot,
 *
 * with a specific level of depth and for a specific move.
 *
 * @param gameSnapshot
 *                     initial game snapshot
 * @param depth
 *              depth of search in the tree
 * @param move
 *             useful move to calculate the new search snapshot
 */
case class InitMsg(gameSnapshot: GameSnapshot, depth: Int, move: Option[Move])

/**
 * The MakeMoveMsg message tells himself to make the move and calculate the new game snapshot
 *
 */
case class MakeMoveMsg()

/**
 * The CheckLeafOrBranchMsg message tells himself to check if a new game snapshot is a terminal node,
 *
 * otherwise he continues to analyze the child nodes
 *
 */
case class CheckLeafOrBranchMsg()

/**
 * The EvaluationMsg message tells himself to evaluate that game snapshot
 *
 */
case class EvaluationMsg()

/**
 * The EvaluationMsg message tells himself to generate the children for the new computed game snapshot
 *
 */
case class GenerateChildrenMsg()

/**
 * The ValueSonMsg message is the message send from children to father with computed score
 *
 * @param score
 *              computed score
 */
case class ValueSonMsg(score: Int)


case class ActorState(gameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef, alfa: Int)

abstract class MiniMaxActor(levelIA: Level) extends Actor {

  override def receive: Receive = {
    case event: InitMsg =>
      context.become(initState(ActorState(event.gameSnapshot, event.depth, event.move, sender(), initAlfa)))
      self ! MakeMoveMsg()
  }

  def initState(actorState: ActorState): Receive = {
    case _: MakeMoveMsg => makeMove(actorState)
  }

  def exploringState(actorState: ActorState): Receive = {
    case _: CheckLeafOrBranchMsg => checkLeafOrBranch(actorState)
  }

  def leafState(actorState: ActorState): Receive = {
    case _: EvaluationMsg => computeEvaluationFunction(actorState.fatherRef, actorState.gameSnapshot)
  }

  def branchState(actorState: ActorState): Receive = {
    case _: GenerateChildrenMsg => generateChildren(actorState)
  }

  def evaluatingChildren(hashMapSonRef: immutable.HashMap[ActorRef, Move], numberOfChildren: Int, bestMove: Option[Move], alfa: Int, fatherRef: ActorRef): Receive = {
    case event: ValueSonMsg => miniMax(hashMapSonRef, numberOfChildren, bestMove, alfa, fatherRef, sender(), event.score)
  }

  def initAlfa: Int

  private def makeMove(actorState: ActorState): Unit = {
    val currentSnapshot = newMoveSnapshot(actorState.gameSnapshot, actorState.move)

    context.become(exploringState(ActorState(currentSnapshot, actorState.depth, actorState.move, actorState.fatherRef,
      actorState.alfa)))
    self ! CheckLeafOrBranchMsg()
  }

  def newMoveSnapshot(fatherSnapshot: GameSnapshot, move: Option[Move]): GameSnapshot =
    if (move.nonEmpty)
      MoveGenerator.makeMove(fatherSnapshot, move.get)
    else
      fatherSnapshot

  def checkLeafOrBranch(actorState: ActorState): Unit =
    if (isTerminalNode(actorState.gameSnapshot.getWinner, actorState.depth)) {
      context.become(leafState(actorState))
      self ! EvaluationMsg()
    } else {
      context.become(branchState(actorState))
      self ! GenerateChildrenMsg()
    }

  def isTerminalNode(gameStatus: Player, depth: Int): Boolean = !gameStatus.equals(Player.None) || depth == 0

  def computeEvaluationFunction(fatherRef: ActorRef, currentGame: GameSnapshot): Unit =
    fatherRef ! ValueSonMsg(EvaluationFunction(currentGame).score(levelIA))

  def generateChildren(actorState: ActorState): Unit = {
    val gamePossibleMoves = MoveGenerator.gamePossibleMoves(actorState.gameSnapshot.getCopy)
    val hashMapSonRef: mutable.HashMap[ActorRef, Move] = mutable.HashMap.empty

    for (possibleMove <- gamePossibleMoves) {
      val sonActor: Props = createChild()
      hashMapSonRef += context.actorOf(sonActor) -> possibleMove
    }

    context.become(evaluatingChildren(toImmutableMap(hashMapSonRef), gamePossibleMoves.size, Option.empty, actorState.alfa, actorState.fatherRef))

    hashMapSonRef.foreach { case (k, v) => k ! InitMsg(actorState.gameSnapshot.getCopy, actorState.depth - 1, Option(v)) }

  }

  def createChild(): Props

  def miniMaxComparison(score: Int, alfa: Int): Boolean

  def miniMax(hashMapSonRef: immutable.HashMap[ActorRef, Move], numberOfChildren: Int, bestMove: Option[Move], alfa: Int,
              fatherRef: ActorRef, sonRef: ActorRef, score: Int): Unit = {
    val newNumberOfChildren = numberOfChildren - 1
    var newAlfa = alfa
    var newBestMove = bestMove
    if (miniMaxComparison(score, alfa)) {
      newAlfa = score
      newBestMove = updateBestMove(hashMapSonRef, sonRef)
    }
    if (newNumberOfChildren == 0) {
      if (newBestMove.nonEmpty)
        fatherRef ! ReturnBestMoveMsg(newBestMove.get)
      else {
        fatherRef ! ValueSonMsg(alfa)
      }
    } else {
      context.become(evaluatingChildren(hashMapSonRef, newNumberOfChildren, newBestMove, newAlfa, fatherRef))
    }
  }

  protected def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option.empty

  private def toImmutableMap(mutableHashMap: mutable.HashMap[ActorRef, Move]): immutable.HashMap[ActorRef, Move] =
    collection.immutable.HashMap(mutableHashMap.toSeq: _*)
}