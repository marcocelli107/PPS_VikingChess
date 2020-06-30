package actor_ia

import akka.actor.{Actor, ActorRef, Props}
import ia.EvaluationFunctionImpl
import model._
import utils.Move

import scala.collection.mutable

// TODO fatherRef sostituibile da sender ???
case class InitMsg(gameSnapshot: GameSnapshot, depth: Int, move: Option[Move])

case class MakeMoveMsg()

case class CheckLeafOrBranchMsg()

case class EvaluationMsg()

case class GenerateChildrenMsg()

case class ValueSonMsg(score: Int)

case class ActorState(gameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef, alfa: Int)

abstract class MiniMaxActor() extends Actor {

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
    case _: EvaluationMsg => computeEvaluationFunction(actorState.fatherRef, actorState.gameSnapshot, actorState.move.get)
  }

  def branchState(actorState: ActorState): Receive = {
    case _: GenerateChildrenMsg => generateChildren(actorState)
  }

  // TODO passare mappa immutabile
  def evaluatingChildren(hashMapSonRef: mutable.HashMap[ActorRef, Move], numberOfChildren: Int, alfa: Int, fatherRef: ActorRef): Receive = {
    case event: ValueSonMsg => miniMax(hashMapSonRef, numberOfChildren, alfa, fatherRef, event.score)
  }

  def initAlfa: Int

  private def makeMove(actorState: ActorState): Unit = {
    val currentSnapshot = newMoveSnapshot(actorState.gameSnapshot, actorState.move)
    context.become(exploringState(ActorState(currentSnapshot, actorState.depth, actorState.move, actorState.fatherRef,
      actorState.alfa)))
    self ! CheckLeafOrBranchMsg()
  }

  def newMoveSnapshot(fatherSnapshot: GameSnapshot, move: Option[Move]): GameSnapshot =
    if(move.nonEmpty)
      MoveGenerator.makeMove(fatherSnapshot, move.get)
    else
      fatherSnapshot

  def checkLeafOrBranch(actorState: ActorState): Unit = actorState.depth match {
    case 0 => context.become(leafState(actorState)); self ! EvaluationMsg()
    case _ => context.become(branchState(actorState)); self ! GenerateChildrenMsg()
  }

  def computeEvaluationFunction(fatherRef: ActorRef, currentGame: GameSnapshot, move: Move): Unit =
    fatherRef ! ValueSonMsg(EvaluationFunctionImpl().score(currentGame, move))


  def generateChildren(actorState: ActorState): Unit = {
    val gamePossibleMoves = MoveGenerator.gamePossibleMoves(actorState.gameSnapshot.getCopy)
    val hashMapSonRef: mutable.HashMap[ActorRef, Move] = mutable.HashMap.empty

    for(possibleMove <- gamePossibleMoves) {
        val sonActor: Props = createChild()
        hashMapSonRef += context.actorOf(sonActor) -> possibleMove
    }

    context.become(evaluatingChildren(hashMapSonRef, gamePossibleMoves.size, actorState.alfa, actorState.fatherRef))

    hashMapSonRef.foreach { case (k, v) => k ! InitMsg(actorState.gameSnapshot.getCopy, actorState.depth - 1, Option(v)) }
  }

  def createChild(): Props

  def miniMaxComparison(score: Int, alfa: Int): Boolean

  def miniMax(hashMapSonRef: mutable.HashMap[ActorRef, Move], numberOfChildren: Int, alfa: Int, fatherRef: ActorRef, score: Int): Unit = {
    val newNumberOfChildren = numberOfChildren - 1
    var newAlfa = alfa
    if(miniMaxComparison(score, alfa)) {
      newAlfa = score
      // TODO updateBestMove
    }
    if(newNumberOfChildren == 0) {
     fatherRef ! ValueSonMsg(alfa)
    } else {
      context.become(evaluatingChildren(hashMapSonRef, newNumberOfChildren, newAlfa, fatherRef))
    }
  }
}

