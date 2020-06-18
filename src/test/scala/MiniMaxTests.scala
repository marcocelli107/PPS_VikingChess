import ia.{GameTree, MiniMax}
import model.{GameVariant, ParserProlog, ParserPrologImpl, Piece, TheoryGame}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import utils.{BoardGame, Pair}
class MiniMaxTests extends FunSuite with MockFactory with Matchers{

  val parser: ParserProlog = ParserPrologImpl(TheoryGame.GameRules.toString)
  var gameTree: GameTree = GameTree()
  val minmax: MiniMax = new MiniMax(gameTree, 3)
  var board: BoardGame.Board = null

  test("Tests calculation of single move of a pawn.") {
    var endCell: BoardGame.BoardCell= null
    var startCell: BoardGame.BoardCell= null
    inSequence {
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      board = minmax.moveAnyPawn(parser, Pair(1,4), Pair(1,2) ).getActualBoard()
      endCell= board.cells.filter(c => c.getCoordinate.equals(Pair(1,2))).head
      startCell= board.cells.filter(c => c.getCoordinate.equals(Pair(1,4))).head
    }
    assert(endCell.getPiece.equals( Piece.BlackPawn))
    assert(startCell.getPiece.equals( Piece.Void))
  }

  test("Tests calculation of a set of moves of a pawn."){
    var setMoves: List[Pair[Int]] = null
    var listParser: List[ParserProlog] = null

    inSequence{
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      setMoves = parser.showPossibleCells( Pair(1,4)).toList
      listParser = minmax.allMovesAnyPawn(parser, Pair(1,4), setMoves, List())
    }
    assert(( listParser.size == setMoves.size))
    listParser.foreach( parserSon =>  assert( !parserSon.equals(parser)))

  }

  test("Tests calculation of all moves of a pawn." ){
    var listCordPawn: List[Pair[Int]] = null
    var listParser: List[ParserProlog] = null

    inSequence{
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      listCordPawn = parser.getActualBoard().cells.filter( cell => cell.getPiece.equals(Piece.BlackPawn)).map( cell => cell.getCoordinate).toList
      listParser = minmax.allMovesAllPawn(parser, listCordPawn, List())
    }
    assert( listParser.size == 116 )
    listParser.foreach( parserSon =>  assert( !parserSon.equals(parser)))

  }



}
