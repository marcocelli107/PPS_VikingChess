package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import ia.{EvaluationFunction, EvaluationFunctionImpl}
import model._
import utils.Move

case class FirstMsg()

case class StartMsg()

case class ValueSonMsg(score: Int)


abstract class MiniMaxActor (gameSnapshot: GameSnapshot, depth:Int, move: Move, fatherRef: ActorRef) extends Actor{

  var numberOfChildren: Int = _
  var tempVal: Int = _
  var evaluationFunction: EvaluationFunction = EvaluationFunctionImpl(gameSnapshot.getBoard.size)
  var myAlfa: Int = _
  var myBeta: Int = _
  var fatherGame: GameSnapshot = gameSnapshot
  var moveGenerator: MoveGenerator = MoveGenerator()
  var gamePossibleMove : List[Move] = List()

  override def receive: Receive = {
    case event: ValueSonMsg => miniMax(event.score)
    case _ : FirstMsg => analyzeMyChildren()
    case _ : StartMsg => compute()
  }

  def compute(): Unit = depth match {
    case 0 => computeEvaluationFunction()
    case _ => analyzeMyChildren()
  }

  def computeEvaluationFunction(): Unit =  fatherRef! ValueSonMsg(evaluationFunction.score(gameSnapshot))


  def analyzeMyChildren():Unit = {


    if(move != null)
      fatherGame = moveGenerator.makeMove(gameSnapshot, move)

    gamePossibleMove = moveGenerator.gamePossibleMoves(gameSnapshot)

    //println(" father game " + fatherGame.getBoard + " coord " + move)


    //println(copy.equals(fatherGame))



    numberOfChildren = gamePossibleMove.size

    var listSonRef: List[ActorRef] = List.empty

    for(pawnMove <- gamePossibleMove ) {
        val sonActor: Props = createChild(fatherGame, pawnMove, self)
        val sonRef = context.actorOf(sonActor)
        listSonRef = listSonRef :+ sonRef
    }

    println("Number of children: " + gamePossibleMove.size)
    listSonRef.foreach( x => x ! StartMsg())

  }

  def createChild(fatherGame: GameSnapshot, move: Move, fatherRef: ActorRef): Props

  def miniMaxComparison(score: Int)

  def miniMax(score: Int): Unit = {
    numberOfChildren = numberOfChildren - 1
    miniMaxComparison(score)
    //println("Alfa: " + myAlfa + " Beta: " + myBeta + " tempVal: " + tempVal)
    if(myBeta <= myAlfa || numberOfChildren == 0) {
      context.children.foreach(child => context.stop(child))
      fatherRef ! ValueSonMsg(tempVal)
      context.stop(self)
    }
  }

}

/** PERFORMANCE HNEFATAFL
  *
  * DEPH 1: circa 0.08 sec
  * DEPH 2: circa 0.8 sec
  * DEPH 3: circa 10 sec
  * DEPH 4: circa 30 sec
  *
  */
object tryProva extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val initGame = game.createGame(GameVariant.Hnefatafl.nameVariant.toLowerCase)
  val gameSnapshot = GameSnapshot(GameVariant.Hnefatafl, initGame._1, initGame._2, initGame._3, Option.empty, 0, 0)
  val system: ActorSystem = ActorSystem()

  val start = System.currentTimeMillis()


  var bestScore : Int = 0

  val father = system.actorOf(Props(FatherActor()))
   father ! StartMsg()

  case class FatherActor() extends Actor {

    override def receive: Receive = {

      case event: ValueSonMsg => println(event.score);  val stop = System.currentTimeMillis() - start
        println(stop)

      case _: StartMsg => system.actorOf(Props(MaxActor(gameSnapshot, 3, -100, 100, null, self))) ! FirstMsg()
    }
  }
}
