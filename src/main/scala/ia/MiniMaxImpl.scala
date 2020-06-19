package ia
import model._
import utils.BoardGame.BoardCell
import utils.Coordinate

trait MiniMax {

  def findBestMove(game: ParserProlog):Coordinate

}

object MiniMaxImpl {
  def apply(depth: Int): MiniMaxImpl = new MiniMaxImpl(depth)
}


class MiniMaxImpl(depth: Int ) extends  MiniMax {

  var evaluationFunction: EvaluationFunction =  EvaluationFunctionImpl()
  type Game = List[ParserProlog]


  override def findBestMove(game: ParserProlog):Coordinate = {
    val gamePossibleMove = getGamePossibleMoves(game, findPlayerPawns(game), List())

    def _findBestMove(game: ParserProlog, gamePossibleMove: List[(Coordinate,Coordinate)] , bestCoord: Coordinate , bestScore: Int ):Coordinate = gamePossibleMove match {
      case Nil  => bestCoord
      case h::t => {
                val makeMove = moveAnyPawn(game.copy(), h._1, h._2)
                val moveScore = pruningAlfaBeta(makeMove, depth, -100,+100, MaxMin.min )
                println ( "OldBestCord " + bestCoord + " OldBestScorec " + moveScore  )
                val (newBestCoord, newBestScore) = if (bestScore > moveScore ) (bestCoord , bestScore) else (h._2, moveScore)
                println ( "NewBestCord " + newBestCoord + " NewBestScorec " + newBestScore  )
               _findBestMove( game,t, newBestCoord, newBestScore )
      }
    }
    _findBestMove(game.copy(), gamePossibleMove, null, -100)
  }


  def pruningAlfaBeta (game: ParserProlog, depth: Int, alfa: Int, beta: Int, phase: MaxMin.Value ): Int =  (depth, phase)  match {
    case (_,_)  if(isTerminalNode(game)) => evaluationFunction.score(game)
    case (0,_)  => evaluationFunction.score(game)
    case (_, MaxMin.Max) => maximizationPhase(game, depth, alfa, beta)
    case  _ => minimizationPhase(game, depth, alfa, beta)
  }

  def maximizationPhase(game: ParserProlog, depth: Int, alfa: Int, beta: Int):Int = {
    val tempVal:Int = -100
    val gameMoves: List[(Coordinate,Coordinate)] = getGamePossibleMoves(game,findPlayerPawns(game), List())

    def _maximizationPhase(fatherGame: ParserProlog, gameMoves: List[(Coordinate,Coordinate)], tempVal: Int, depth: Int, alfa: Int, beta: Int): Int = gameMoves match {
      case Nil => tempVal
      case _  if beta <= alfa => tempVal
      case h::t =>  {
            val sonGame: ParserProlog = moveAnyPawn(fatherGame.copy(), h._1, h._2)
            val newTempVal = max(tempVal, pruningAlfaBeta(sonGame, depth-1, alfa, beta, MaxMin.min))
            val newAlfa = max(alfa, newTempVal)
            //println("Max: " + depth)
            _maximizationPhase(fatherGame, t, newTempVal, depth, newAlfa, beta)
      }
    }

    _maximizationPhase(game, gameMoves,tempVal,depth,alfa,beta)
  }

  def minimizationPhase(game: ParserProlog, depth: Int, alfa: Int, beta: Int): Int = {
    val tempVal:Int = 100
    val gameMoves: List[(Coordinate,Coordinate)] = getGamePossibleMoves(game,findPlayerPawns(game), List())

    def _minimizationPhase(fatherGame: ParserProlog, gameMoves: List[(Coordinate,Coordinate)], tempVal: Int, depth: Int, alfa: Int, beta: Int): Int = gameMoves match {
      case Nil => tempVal
      case _  if beta <= alfa => tempVal
      case h::t =>  {
            val sonGame: ParserProlog = moveAnyPawn(fatherGame.copy(), h._1, h._2)
            val newTempVal = min(tempVal, pruningAlfaBeta(sonGame, depth - 1, alfa, beta, MaxMin.Max))
            val newBeta = min(beta, newTempVal)
              //println("Min: "+depth)
            _minimizationPhase(fatherGame, t, newTempVal, depth, alfa, newBeta)
      }
    }

    _minimizationPhase(game, gameMoves,tempVal,depth,alfa,beta)
  }

  def getPlayerAndBoard(parserProlog: ParserProlog ):(Player.Value, List[BoardCell] ) = {
    (parserProlog.getPlayer, parserProlog.getActualBoard.cells.toList)
  }

  def findPlayerPawns(parserProlog: ParserProlog): List[Coordinate] = {
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

  def allMovesAllPawn(parserProlog: ParserProlog, listCordPawn: List[Coordinate], listParser: Game  ):Game = listCordPawn match {
    case Nil => listParser
    case h::t => allMovesAllPawn( parserProlog,
      t,
      listParser ++ allMovesAnyPawn(parserProlog.copy(), h,getPawnPossibleMove(h, parserProlog.copy()), List()))
  }

  def allMovesAnyPawn(parserProlog: ParserProlog, startCord: Coordinate, listEndCord: List[Coordinate], listParser: Game):Game = listEndCord match {
    case Nil => listParser
    case h::t => allMovesAnyPawn(parserProlog,startCord, t, listParser :+ moveAnyPawn(parserProlog.copy(), startCord,h))
  }

  def moveAnyPawn(parserProlog: ParserProlog, startCord: Coordinate, endCord: Coordinate ): ParserProlog = {
    parserProlog.makeLegitMove(startCord,endCord)
    parserProlog.copy()
  }

  def isTerminalNode(game:ParserProlog):Boolean = game.hasWinner.nonEmpty

  def getGamePossibleMoves(parserProlog: ParserProlog, pawnsPlayer: List[Coordinate], gamePossibleMoves: List[(Coordinate,Coordinate) ] ) : List[(Coordinate,Coordinate) ] = pawnsPlayer match {
    case Nil => gamePossibleMoves
    case h::t => getGamePossibleMoves(parserProlog, t, getPawnPossibleMovesMap(h, parserProlog) ++ gamePossibleMoves )
  }

  def getPawnPossibleMovesMap( pawnCord: Coordinate, parserProlog: ParserProlog): List[(Coordinate,Coordinate)] = {
    getPawnPossibleMove(pawnCord, parserProlog).map( endCord => (pawnCord,endCord ))
  }

  def getPawnPossibleMove(coordinate: Coordinate, parserProlog: ParserProlog): List[Coordinate] = {
    parserProlog.showPossibleCells(coordinate).toList
  }

  def max(x: Int, y: Int ): Int = if (x > y)  x else y

  def min( x : Int , y: Int): Int = if (x < y ) x else y

  def isOwner(pawn:Piece.Value, player: Player.Value): Boolean = ( pawn, player) match {
    case ( Piece.WhitePawn, Player.White) => true
    case ( Piece.WhiteKing, Player.White) => true
    case ( Piece.BlackPawn, Player.Black) => true
    case _ => false
  }

}
object TryMinMax extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  val board = parserProlog.createGame(GameVariant.Brandubh.nameVariant.toLowerCase)._3
  var ef: EvaluationFunction =  EvaluationFunctionImpl()
  //  var gameTree: GameTree = GameTree()
  val miniMax: MiniMaxImpl = new MiniMaxImpl(  2)

  println( parserProlog.hasWinner)

}