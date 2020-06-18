package ia
import model._
import utils.BoardGame.BoardCell
import utils.Coordinate

class MiniMax(  level: Int ) {

  var parserHistory: List[ParserProlog] = List()

  type Game = List[ParserProlog]

  def createBufferedList(coordinate: Coordinate, parserProlog: ParserProlog): List[Coordinate] = {
  parserProlog.showPossibleCells(coordinate).toList
  }

  def isOwner(pawn:Piece.Value, player: Player.Value): Boolean = ( pawn, player) match {
    case ( Piece.WhitePawn, Player.White) => true
    case ( Piece.WhiteKing, Player.White) => true
    case ( Piece.BlackPawn, Player.Black) => true
    case _ => false
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
    case h::t => allMovesAllPawn( parserProlog.copy(), t,
                                  listParser ++ allMovesAnyPawn(parserProlog.copy(), h,
                                  createBufferedList(h, parserProlog.copy()), List()))
  }

  def allMovesAnyPawn(parserProlog: ParserProlog,startCord: Coordinate, listEndCord: List[Coordinate], listParser: Game):Game = listEndCord match {
    case Nil => listParser
    case h::t => allMovesAnyPawn(parserProlog.copy(),startCord, t, listParser :+ moveAnyPawn(parserProlog.copy(), startCord,h))
  }

  def moveAnyPawn(parserProlog: ParserProlog, startCord: Coordinate, endCord: Coordinate ): ParserProlog = {
    parserProlog.makeMove(startCord,endCord)
    parserProlog.copy()
  }

}
object TryMinMax extends App{
  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)
  val board = parserProlog.createGame(GameVariant.Brandubh.nameVariant.toLowerCase)._3
//  var gameTree: GameTree = GameTree()
  val miniMax = new MiniMax(  2)

  println( miniMax.findPlayerPawns(parserProlog ) )

}



