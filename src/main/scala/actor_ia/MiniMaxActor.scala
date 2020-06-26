package actor_ia

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import alice.tuprolog.NoMoreSolutionException
import ia.{EvaluationFunction, EvaluationFunctionImpl}
import model.{GameVariant, ParserProlog, ParserPrologImpl, TheoryGame}
import utils.{Coordinate, Move}

case class FirstMsg()

case class StartMsg()

case class ValueSonMsg(score: Int)


abstract class MiniMaxActor (game: ParserProlog, depth:Int, move: (Coordinate,Coordinate), fatherRef: ActorRef) extends Actor{

  var numberOfChildren: Int = _
  var tempVal: Int = _
  var evaluationFunction: EvaluationFunction = EvaluationFunctionImpl(game.getActualBoard.size)
  var myAlfa: Int = _
  var myBeta: Int = _
  var fatherGame: ParserProlog = game
  var gamePossibleMove : List[(Coordinate,Coordinate)] = List()

  override def receive: Receive = {
    case event: ValueSonMsg => miniMax(event.score)
    case _ : FirstMsg => analyzeMyChildren()
    case _ : StartMsg => compute()
  }

  def compute(): Unit = depth match {
    case 0 => computeEvaluationFunction()
    case _ => analyzeMyChildren()
  }

  def computeEvaluationFunction(): Unit =  fatherRef! ValueSonMsg(evaluationFunction.score(game))


  def analyzeMyChildren():Unit = {

    try {
      if(move != null) {
        fatherGame = moveAnyPawn(game, move._1, move._2)
      }
      gamePossibleMove = fatherGame.gamePossibleMoves()

    }catch {
      case _: NoMoreSolutionException => println(" father game " + fatherGame.getActualBoard + " coord " + move)
    }

    //println(copy.equals(fatherGame))



    numberOfChildren = gamePossibleMove.size

    var listSonRef:List[ActorRef] = List()

    for(pawnMove <- gamePossibleMove ) {
        val sonActor: Props = createChild(fatherGame, pawnMove, self)
        val sonRef = context.actorOf(sonActor)
        listSonRef = listSonRef :+ sonRef
    }

   // println("Number of children: " + gamePossibleMove.size)
    listSonRef.foreach( x => x ! StartMsg())

  }

  def createChild(fatherGame: ParserProlog, move: (Coordinate, Coordinate), fatherRef: ActorRef): Props

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

  def  moveAnyPawn(parserProlog: ParserProlog, startCord: Coordinate, endCord: Coordinate ): ParserProlog = {
    val copy = parserProlog.copy()
    copy.makeLegitMove(Move(startCord, endCord))
    copy
  }
}


object tryProva extends App {
  val THEORY: String = TheoryGame.GameRules.toString
  val game: ParserProlog = ParserPrologImpl(THEORY)
  val board = game.createGame(GameVariant.Hnefatafl.nameVariant.toLowerCase)._3
  val system: ActorSystem = ActorSystem()
  val firstMove = game.gamePossibleMoves()

  var bestScore : Int = 0

  val father = system.actorOf(Props(FatherActor()))
   father ! StartMsg()

  case class FatherActor() extends Actor{

    override def receive: Receive = {

      case event: ValueSonMsg => println(event.score)

      case _: StartMsg => system.actorOf( Props(MaxActor(game, 3, -100, 100, null, self))) ! FirstMsg()
    }
  }

}
