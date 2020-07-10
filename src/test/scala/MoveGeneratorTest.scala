import model.game.Player.Player
import model.game.GameSnapshot.GameSnapshotImpl
import model.game.{BoardGame, Coordinate, GameSnapshot, GameVariant, Move, MoveGenerator, Piece, Player}
import model.prolog.ParserProlog
import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class MoveGeneratorTest  extends FunSuite with MockFactory with Matchers {

  var game: (Player, Player, BoardGame.Board, Int) = _
  var snapshot: GameSnapshot = _

  test("Test number of possible moves - Brandubh."){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    assert(MoveGenerator.gamePossibleMoves(snapshot).size == 40)
  }

  test("Test possible moves - Brandubh."){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    val possibleMove_Brandubh = List(Move(Coordinate(1,4),Coordinate(1,5)),
      Move(Coordinate(1,4),Coordinate(1,6)), Move(Coordinate(1,4),Coordinate(1,3)),
      Move(Coordinate(1,4),Coordinate(1,2)), Move(Coordinate(2,4),Coordinate(2,5)),
      Move(Coordinate(2,4),Coordinate(2,6)), Move(Coordinate(2,4),Coordinate(2,7)),
      Move(Coordinate(2,4),Coordinate(2,3)),  Move(Coordinate(2,4),Coordinate(2,2)),
      Move(Coordinate(2,4),Coordinate(2,1)), Move(Coordinate(4,1),Coordinate(3,1)),
      Move(Coordinate(4,1),Coordinate(2,1)), Move(Coordinate(4,1),Coordinate(5,1)),
      Move(Coordinate(4,1),Coordinate(6,1)), Move(Coordinate(4,2),Coordinate(3,2)),
      Move(Coordinate(4,2),Coordinate(2,2)), Move(Coordinate(4,2),Coordinate(1,2)),
      Move(Coordinate(4,2),Coordinate(5,2)), Move(Coordinate(4,2),Coordinate(6,2)),
      Move(Coordinate(4,2),Coordinate(7,2)), Move(Coordinate(4,6),Coordinate(3,6)),
      Move(Coordinate(4,6),Coordinate(2,6)), Move(Coordinate(4,6),Coordinate(1,6)),
      Move(Coordinate(4,6),Coordinate(5,6)), Move(Coordinate(4,6),Coordinate(6,6)),
      Move(Coordinate(4,6),Coordinate(7,6)), Move(Coordinate(4,7),Coordinate(3,7)),
      Move (Coordinate(4,7),Coordinate(2,7)), Move(Coordinate(4,7),Coordinate(5,7)),
      Move (Coordinate(4,7),Coordinate(6,7)), Move(Coordinate(6,4),Coordinate(6,5)),
      Move(Coordinate(6,4),Coordinate(6,6)), Move(Coordinate(6,4),Coordinate(6,7)),
      Move(Coordinate(6,4),Coordinate(6,3)), Move(Coordinate(6,4),Coordinate(6,2)),
      Move(Coordinate(6,4),Coordinate(6,1)), Move(Coordinate(7,4),Coordinate(7,5)),
      Move(Coordinate(7,4),Coordinate(7,6)), Move(Coordinate(7,4),Coordinate(7,3)),
      Move(Coordinate(7,4),Coordinate(7,2)))

    MoveGenerator.gamePossibleMoves(snapshot).foreach(move => assert(possibleMove_Brandubh.contains(move)))
  }


  test("Test number of possible moves - Hnefatafl."){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    assert(MoveGenerator.gamePossibleMoves(snapshot).size == 116)
  }

  test("Make moves from (1,4) to (4,4) - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot,Move(Coordinate(1,4), Coordinate(4,4)))
    assert(snapshot.getBoard.getCell(Coordinate(4,4)).getPiece.equals( Piece.BlackPawn))
  }

  test( "Vertical captured white test - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(6,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(4,6)))

    assert(snapshot.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty))
    assert(snapshot.getNumberCapturedWhites == 1)
  }

  test( "Vertical captured black test - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,6)))

    assert(snapshot.getBoard.getCell(Coordinate(4,6)).getPiece.equals(Piece.Empty))
    assert(snapshot.getNumberCapturedBlacks == 1)
  }

  test( "Double captured white test - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,4), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,9), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,6)))

    assert(snapshot.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty) &&
      snapshot.getBoard.getCell(Coordinate(7,6)).getPiece.equals(Piece.Empty) &&
      snapshot.getNumberCapturedWhites == 2)
  }


  test( "Triple captured white test - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,4), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,9), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,6)))

    assert(snapshot.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty) &&
      snapshot.getBoard.getCell(Coordinate(7,6)).getPiece.equals(Piece.Empty) &&
      snapshot.getBoard.getCell(Coordinate(6,5)).getPiece.equals(Piece.Empty))
    assert(snapshot.getNumberCapturedWhites == 3)
  }

  test( "Triple captured white test - Tawlbwrdd"){
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,7), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(10,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(4,5)))


    assert(snapshot.getBoard.getCell(Coordinate(4,4)).getPiece.equals(Piece.Empty) &&
      snapshot.getBoard.getCell(Coordinate(4,6)).getPiece.equals(Piece.Empty) &&
      snapshot.getBoard.getCell(Coordinate(5,5)).getPiece.equals(Piece.Empty))
    assert(snapshot.getNumberCapturedWhites == 3)
  }

  test("1. Test white win in (1,1) - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(1,1)))

    assert(snapshot.getWinner.equals(Player.White))
  }

  test("2. Test white win in (1,11) - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(1,11)))

    assert(snapshot.getWinner.equals(Player.White))
  }

  test("3. Test white win in (11,11) - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(11,11)))

    assert(snapshot.getWinner.equals(Player.White))
  }

  test("4. Test white win in (11,1) - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(11,1)))

    assert(snapshot.getWinner.equals(Player.White))
  }

  test("Test board king captured 3 sides and throne - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(10,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(10,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,4), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,4), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,5)))

    assert(snapshot.getWinner.equals(Player.Black))
  }

  test("Test board king far from throne, not been captured 3 sides - Tawlbwrdd"){
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(3,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(5,2)))

    assert(snapshot.getWinner.equals(Player.None))
  }


  test("Test board 11 king not captured on edge - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(2,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,1), Coordinate(5,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,2), Coordinate(6,2)))

    assert(snapshot.getWinner.equals(Player.None))
  }

  test("Test board 11 king on throne capture - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(1,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(10,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,10), Coordinate(6,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,6), Coordinate(9,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,7), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,11), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(7,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(9,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,4), Coordinate(10,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(7,6)))

    assert(snapshot.getWinner.equals(Player.Black) && snapshot.getBoard.getCell(Coordinate(6,6)).getPiece.equals(Piece.WhiteKing))
  }

  test("Test board 9 king on throne capture - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(3,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(7,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,9), Coordinate(4,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,9)))
    assert(snapshot.getBoard.getCell(Coordinate(7,5)).getPiece.equals(Piece.WhitePawn))

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,1), Coordinate(6,5)))
    assert(snapshot.getBoard.getCell(Coordinate(7,5)).getPiece.equals(Piece.Empty))

    assert(snapshot.getNumberCapturedWhites == 1)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(8,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(1,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,4)))


    assert(snapshot.getWinner.equals(Player.Black) && snapshot.getBoard.getCell(Coordinate(5,5)).getPiece.equals(Piece.WhiteKing))

  }


  test("Test board 7 king on throne no captured three sides - Brandubh"){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(5,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,5)))

    assert(snapshot.getWinner.equals(Player.None))
  }


  test("Test board 9 king far from throne, vert capture - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(3,3)))

    assert(snapshot.getWinner.equals(Player.Black))
  }

  test("Test board 9 king far from throne, horizontal capture - Tablut") {
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6, 9), Coordinate(8, 9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5, 5), Coordinate(8, 8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8, 5), Coordinate(8, 7)))

    assert(snapshot.getWinner.equals(Player.Black))
  }

  test("Test board 9 sneaky king not captured - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(7,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(6,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(6,8)))

    assert(snapshot.getWinner.equals(Player.None))
  }

  test("Test board 11 sneaky king not captured - Tawlbwrdd"){
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(7,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,9), Coordinate(8,9)))

    assert(snapshot.getWinner.equals(Player.None))
  }

  test("Test board 7 king next to throne captured three sides - Brandubh"){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(3,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(5,3)))

    assert(snapshot.getWinner.equals(Player.Black))
  }

  test("Test king not captured on 2 sides next to throne - Brandubh") {
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(5,5)))

    assert(snapshot.getWinner.equals(Player.None))
  }

  test("Test board 7 Draw - Brandubh"){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(5,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(1,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,7), Coordinate(5,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(4,6)))

    assert(snapshot.getWinner.equals(Player.Draw))
  }

  test("Test board 9 Draw - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(7,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(1,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(1,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(1,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,2), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,8), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(6,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,1), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,7), Coordinate(1,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(2,6)))

    assert(snapshot.getWinner.equals(Player.Draw))
  }

  test("Test board 11 Draw - Hnefatafl"){
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshotImpl(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,2), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(2,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,7), Coordinate(3,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(4,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(7,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,11), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(7,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,3), Coordinate(8,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,9), Coordinate(7,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,9), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,7), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,7), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(9,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,10), Coordinate(8,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,6), Coordinate(10,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,11), Coordinate(9,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,8), Coordinate(9,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,10), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,8), Coordinate(9,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,11), Coordinate(9,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,6), Coordinate(10,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,10), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,6), Coordinate(10,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,11), Coordinate(10,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,9), Coordinate(9,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,11), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,10), Coordinate(10,11)))

    assert(snapshot.getWinner.equals(Player.Draw))
  }

}