package ia
import model.MaxMin.MaxMin
import model.{MaxMin, _}
import utils.BoardGame.BoardCell
import utils.Pair

case class Node(gameState: ParserProlog, score: Option[Int])

class MiniMax( level: Int ) {

  var parserHistory: List[ParserProlog] = List()
  var evaluationFunction: EvaluationFunction =  EvaluationFunctionImpl()

  type Game = List[ParserProlog]

  def createBufferedList(coordinate: Pair[Int], parserProlog: ParserProlog): List[Pair[Int]] = {
  parserProlog.showPossibleCells(coordinate).toList
  }

  def isOwner(pawn:Piece.Value, player: Player.Value): Boolean = ( pawn, player) match {
    case ( Piece.WhitePawn, Player.White) => true
    case ( Piece.WhiteKing, Player.White) => true
    case ( Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

  def getPlayerAndBoard(parserProlog: ParserProlog ):(Player.Value, List[BoardCell] ) = {
     (parserProlog.getPlayer(), parserProlog.getActualBoard().cells.toList)
  }

  def findPlayerPawns(parserProlog: ParserProlog): List[Pair[Int]] = {
    val (player, boardCells ) = getPlayerAndBoard(parserProlog)
    for {
      cell <- boardCells
      if isOwner(cell.getPiece, player)
    } yield cell.getCoordinate
  }

  def computeAllGame( parserProlog: ParserProlog,  moves:Int ): List[Game] = moves match {
    case 1 => List( allMovesAllPawn(parserProlog.copy(), findPlayerPawns(parserProlog.copy), List()) )
    case _ => for {
      game <- computeAllGame( parserProlog.copy(), moves-1)
      possibleParserProlog <- game
      newGame = allMovesAllPawn(possibleParserProlog.copy(), findPlayerPawns(possibleParserProlog.copy), List())
    } yield newGame ::: game
  }

  def computeAnyGame(listParserProlog: Game , levelGames: List[Game] ): List[Game] = listParserProlog match {
    case Nil => levelGames
    case h::t => computeAnyGame(t, levelGames :+allMovesAllPawn(h.copy(), findPlayerPawns(h.copy()), List()))
  }

  def allMovesAllPawn(parserProlog: ParserProlog, listCordPawn: List[Pair[Int]], listParser: Game  ):Game = listCordPawn match {
    case Nil => listParser
    case h::t => allMovesAllPawn( parserProlog,
                                  t,
                                  listParser ++ allMovesAnyPawn(parserProlog.copy(), h,createBufferedList(h, parserProlog.copy()), List()))
  }

  def allMovesAnyPawn(parserProlog: ParserProlog, startCord: Pair[Int], listEndCord: List[Pair[Int]], listParser: Game):Game = listEndCord match {
    case Nil => listParser
    case h::t => allMovesAnyPawn(parserProlog,startCord, t, listParser :+ moveAnyPawn(parserProlog.copy(), startCord,h))
  }

  def moveAnyPawn(parserProlog: ParserProlog, startCord: Pair[Int], endCord: Pair[Int] ): ParserProlog = {
    parserProlog.makeMove(startCord,endCord)
    parserProlog.copy()
  }

  def pruningAlfaBeta (node: Node, depth: Int,  alfa: Double , beta: Double, phase: MaxMin ): Int =  (depth, phase)  match {
    case (_,_)  if(isTerminalNode(node)) => evaluationFunction.score(node.gameState)
    case (0,_)  => ???
    case (_, MaxMin.Max) => maximizationPhase()
    case  _ => minimizationPhase()
  }

  def maximizationPhase():Int = ???

  def minimizationPhase(): Int = ???

  def isTerminalNode(node:Node):Boolean = ???


}
object TryMinMax extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  val board = parserProlog.createGame(GameVariant.Brandubh.nameVariant.toLowerCase)._3
//  var gameTree: GameTree = GameTree()
  val miniMax = new MiniMax(  2)

  println( miniMax.findPlayerPawns(parserProlog ) )

}



