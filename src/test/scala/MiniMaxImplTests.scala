import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
class MiniMaxImplTests extends FunSuite with MockFactory with Matchers{

 /* val parser: ParserProlog = ParserPrologImpl(TheoryGame.GameRules.toString)

  val minmax: MiniMaxImpl = new MiniMaxImpl(3)
  var board: BoardGame.Board = null

  test("Tests calculation of single move of a pawn.") {
    var endCell: BoardGame.BoardCell= null
    var startCell: BoardGame.BoardCell= null
    inSequence {
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      board = minmax.moveAnyPawn(parser, Coordinate(1,4), Coordinate(1,2) ).getActualBoard
      endCell= board.cells.filter(c => c.getCoordinate.equals(Coordinate(1,2))).head
      startCell= board.cells.filter(c => c.getCoordinate.equals(Coordinate(1,4))).head
    }
    assert(endCell.getPiece.equals( Piece.BlackPawn))
    assert(startCell.getPiece.equals( Piece.Empty))
  }

  test("Tests calculation of a set of moves of a pawn."){
    var setMoves: List[Coordinate] = null
    var listParser: List[ParserProlog] = null

    inSequence{
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      setMoves = parser.showPossibleCells( Coordinate(1,4)).toList
      listParser = minmax.allMovesAnyPawn(parser, Coordinate(1,4), setMoves, List())
    }
    assert(( listParser.size == setMoves.size))
    listParser.foreach( parserSon =>  assert( !parserSon.equals(parser)))

  }

  test("Tests calculation of all moves of a pawn." ){
    var listCordPawn: List[Coordinate] = null
    var listParser: List[ParserProlog] = null

    inSequence{
      parser.createGame(GameVariant.Hnefatafl.toString().toLowerCase())
      listCordPawn = parser.getActualBoard.cells.filter( cell => cell.getPiece.equals(Piece.BlackPawn)).map( cell => cell.getCoordinate).toList
      listParser = minmax.allMovesAllPawn(parser, listCordPawn, List())
    }
    assert( listParser.size == 116 )
    listParser.foreach( parserSon =>  assert( !parserSon.equals(parser)))

  }*/



}
