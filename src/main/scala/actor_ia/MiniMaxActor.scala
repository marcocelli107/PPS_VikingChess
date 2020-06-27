package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import ia.{EvaluationFunction, EvaluationFunctionImpl}
import model._
import utils.Move

case class FirstMsg()

case class StartMsg()

case class ValueSonMsg(score: Int)


abstract class MiniMaxActor (fatherGameSnapshot: GameSnapshot, depth: Int, move: Option[Move], fatherRef: ActorRef) extends Actor {

  var numberOfChildren: Int = _
  var tempVal: Int = _
  var evaluationFunction: EvaluationFunction = EvaluationFunctionImpl(fatherGameSnapshot.getBoard.size)
  var myAlfa: Int = _
  var myBeta: Int = _
  var currentGame: GameSnapshot = fatherGameSnapshot
  var moveGenerator: MoveGenerator = MoveGenerator()
  var gamePossibleMove : List[Move] = List()

  override def receive: Receive = {
    case event: ValueSonMsg => miniMax(event.score)
    case _: FirstMsg => analyzeMyChildren()
    case _: StartMsg => compute()
  }

  def compute(): Unit = depth match {
    case 0 => computeEvaluationFunction()
    case _ => analyzeMyChildren()
  }

  def computeEvaluationFunction(): Unit =  fatherRef ! ValueSonMsg(evaluationFunction.score(fatherGameSnapshot))


  def analyzeMyChildren(): Unit = {

    if(move.nonEmpty)
      currentGame = moveGenerator.makeMove(fatherGameSnapshot, move.get)

    gamePossibleMove = moveGenerator.gamePossibleMoves(currentGame)

    numberOfChildren = gamePossibleMove.size

    //println("N° Actors: " + numberOfChildren)

    var listSonRef: List[ActorRef] = List.empty

    for(pawnMove <- gamePossibleMove) {
        val sonActor: Props = createChild(currentGame.getCopy, pawnMove, this.self)
        listSonRef = listSonRef :+ context.actorOf(sonActor)
    }

    listSonRef.foreach( _ ! StartMsg())
  }

  def createChild(currentGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props

  def miniMaxComparison(score: Int)

  def miniMax(score: Int): Unit = {
    numberOfChildren = numberOfChildren - 1
    miniMaxComparison(score)
    if(myBeta <= myAlfa || numberOfChildren == 0) {
      context.children.foreach(child => context.stop(child))
      fatherRef ! ValueSonMsg(tempVal)
      context.stop(self)
    }
  }

}

/** PERFORMANCE HNEFATAFL
  *
  * DEPH 1: HNEFATAFL - circa 0.08 sec / TAWLBWURDD - circa 0.07 sec / TABLUT - circa 0.06 sec / BRANDUBH - circa 0.05 sec
  * DEPH 2: HNEFATAFL - circa 0.8 sec / TAWLBWURDD - circa 1.11 sec / TABLUT - circa 0.76 sec / BRANDUBH - circa 0.44 sec
  * DEPH 3: HNEFATAFL - circa 8 sec / TAWLBWURDD - circa 10 sec / TABLUT - circa 4 sec / BRANDUBH - circa 1.4 sec
  *
  * ADESSO VA MA CON 9x9 e SUPERIORI CI METTE TROPPO
  * DEPH 4: HNEFATAFL - circa ... sec / TAWLBWURDD - circa ... sec / TABLUT - circa ... sec / BRANDUBH - circa 17 sec
  *
  */
object tryProva extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Tawlbwrdd.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Tawlbwrdd, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)
  val system: ActorSystem = ActorSystem()

  val start = System.currentTimeMillis()


  var bestScore : Int = 0

  val father = system.actorOf(Props(FatherActor()))
   father ! StartMsg()

  case class FatherActor() extends Actor {

    override def receive: Receive = {

      case event: ValueSonMsg => println(event.score);  val stop = System.currentTimeMillis() - start
        println(stop)

      case _: StartMsg => system.actorOf(Props(MaxActor(gameSnapshot, 3, -100, 100, Option.empty, self))) ! FirstMsg()
    }
  }
}
