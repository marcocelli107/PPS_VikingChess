package ia.minimax

import akka.actor.{Actor, ActorRef, Props}
import ia.evaluation_function.EvaluationFunction
import ia.messages.Messages.ReturnBestMoveMsg
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

/**
  * State of the actor in current instant of computation
  *
  * @param gameSnapshot
  *                     game state
  *
  * @param depth
  *              depth of exploration according to difficulty
  *
  * @param move
  *             move chosen by child actor
  *
  * @param fatherRef
  *                  father reference
  *
  * @param alpha
  *              value used for comparison in minimax
  */
case class ActorState(gameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef, alpha: Int)

abstract class MiniMaxActor(levelIA: Level) extends Actor {

  override def receive: Receive = {
    case event: InitMsg =>
      context.become(initState(ActorState(event.gameSnapshot, event.depth, event.move, sender(), initAlpha)))
      self ! MakeMoveMsg()
  }

  /**
   * Initial state of each Actor in which waits for MakeMoveMsg request
   *
   * @param actorState
   *                  actor state
   */
  def initState(actorState: ActorState): Receive = {
    case _: MakeMoveMsg => makeMove(actorState)
  }

  /**
   * State of the actor in which choose if exploration is in a branch or in a leaf
   *
   * @param actorState
   *                  actor state
   */
  def exploringState(actorState: ActorState): Receive = {
    case _: CheckLeafOrBranchMsg => checkLeafOrBranch(actorState)
  }

  /**
   * State of the actor in which exploration stops and score of the leaf is evaluated
   *
   * @param actorState
   *                  actor state
   */
  def leafState(actorState: ActorState): Receive = {
    case _: EvaluationMsg => computeEvaluationFunction(actorState.fatherRef, actorState.gameSnapshot)
  }

  /**
   * State of the actor in which exploration continues and he generates new children
   *
   * @param actorState
   *                  actor state
   */
  def branchState(actorState: ActorState): Receive = {
    case _: GenerateChildrenMsg => generateChildren(actorState)
  }

  /**
    * State of the actor in which waits his children evaluation and makes minimax comparison
    *
    * @param hashMapSonRef
    *                      Map reference of his children
    *
    * @param numberOfChildren
    *                         number of his children
    *
    * @param bestMove
    *                 best move
    *
    * @param alpha
    *             value to check for minimax algorithm
    *
    * @param fatherRef
    *                  reference to the father
    */
  def evaluatingChildren(hashMapSonRef: immutable.HashMap[ActorRef, Move], numberOfChildren: Int, bestMove: Option[Move], alpha: Int, fatherRef: ActorRef): Receive = {
    case event: ValueSonMsg => miniMax(hashMapSonRef, numberOfChildren, bestMove, alpha, fatherRef, sender(), event.score)
  }

  /**
   * Method that creates child that will be extended by Min/Max Actors
   */
  def createChild(): Props

  /**
   * Method that compares values to determine best score that will be extended by Min/Max Actors
   */
  def miniMaxComparison(score: Int, alpha: Int): Boolean

  /**
   * Method that initialize alpha value that will be extended by Min/Max Actors
   */
  def initAlpha: Int

  private def makeMove(actorState: ActorState): Unit = {
    val currentSnapshot = newMoveSnapshot(actorState.gameSnapshot, actorState.move)

    context.become(exploringState(ActorState(currentSnapshot, actorState.depth, actorState.move, actorState.fatherRef,
      actorState.alpha)))
    self ! CheckLeafOrBranchMsg()
  }

  private def newMoveSnapshot(fatherSnapshot: GameSnapshot, move: Option[Move]): GameSnapshot =
    if (move.nonEmpty)
      MoveGenerator.makeMove(fatherSnapshot, move.get)
    else
      fatherSnapshot

  private def checkLeafOrBranch(actorState: ActorState): Unit =
    if (isTerminalNode(actorState.gameSnapshot.getWinner, actorState.depth)) {
      context.become(leafState(actorState))
      self ! EvaluationMsg()
    } else {
      context.become(branchState(actorState))
      self ! GenerateChildrenMsg()
    }

  private def isTerminalNode(gameStatus: Player, depth: Int): Boolean = !gameStatus.equals(Player.None) || depth == 0

  private def computeEvaluationFunction(fatherRef: ActorRef, currentGame: GameSnapshot): Unit =
    fatherRef ! ValueSonMsg(EvaluationFunction(currentGame).score(levelIA))

  private def generateChildren(actorState: ActorState): Unit = {
    val gamePossibleMoves = MoveGenerator.gamePossibleMoves(actorState.gameSnapshot.getCopy)
    val hashMapSonRef: mutable.HashMap[ActorRef, Move] = mutable.HashMap.empty

    for (possibleMove <- gamePossibleMoves) {
      val sonActor: Props = createChild()
      hashMapSonRef += context.actorOf(sonActor) -> possibleMove
    }

    context.become(evaluatingChildren(toImmutableMap(hashMapSonRef), gamePossibleMoves.size, Option.empty, actorState.alpha, actorState.fatherRef))

    hashMapSonRef.foreach { case (k, v) => k ! InitMsg(actorState.gameSnapshot.getCopy, actorState.depth - 1, Option(v)) }

  }

  private def miniMax(hashMapSonRef: immutable.HashMap[ActorRef, Move], numberOfChildren: Int, bestMove: Option[Move], alpha: Int,
              fatherRef: ActorRef, sonRef: ActorRef, score: Int): Unit = {
    val newNumberOfChildren = numberOfChildren - 1
    var newAlpha = alpha
    var newBestMove = bestMove
    if (miniMaxComparison(score, alpha)) {
      newAlpha = score
      newBestMove = updateBestMove(hashMapSonRef, sonRef)
    }
    if (newNumberOfChildren == 0) {
      if (newBestMove.nonEmpty)
        fatherRef ! ReturnBestMoveMsg(newBestMove.get)
      else {
        fatherRef ! ValueSonMsg(alpha)
      }
    } else {
      context.become(evaluatingChildren(hashMapSonRef, newNumberOfChildren, newBestMove, newAlpha, fatherRef))
    }
  }

  protected def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option.empty

  private def toImmutableMap(mutableHashMap: mutable.HashMap[ActorRef, Move]): immutable.HashMap[ActorRef, Move] =
    collection.immutable.HashMap(mutableHashMap.toSeq: _*)
}