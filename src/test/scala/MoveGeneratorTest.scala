import actor_ia.MoveGenerator
import model.GameSnapshot.GameSnapshotImpl
import model.{GameVariant, ParserProlog, ParserPrologImpl, Piece, Player, TheoryGame}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import utils.{BoardGame, Coordinate, Move}

class MoveGeneratorTest  extends FunSuite with MockFactory with Matchers {

  var moveGenerator: MoveGenerator.type = actor_ia.MoveGenerator

  val THEORY: String = TheoryGame.GameRules.toString
  val parserProlog: ParserProlog = ParserPrologImpl(THEORY)

  var variantBrandubh: GameVariant.Val = GameVariant.Brandubh
  var variantHnefatafl: GameVariant.Val = GameVariant.Hnefatafl
  var variantTablut:GameVariant.Val = GameVariant.Tablut
  var variantTawlbwrdd:GameVariant.Val = GameVariant.Tawlbwrdd

  var gameBrandubh: (Player.Val, Player.Val, BoardGame.Board, Int) = parserProlog.createGame(variantBrandubh.toString().toLowerCase)
  var gameHnefatafl: (Player.Val, Player.Val, BoardGame.Board, Int) = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
  var gameTablut: (Player.Val, Player.Val, BoardGame.Board, Int) = parserProlog.createGame(variantTablut.toString().toLowerCase)
  var gameTawlbwrdd: (Player.Val, Player.Val, BoardGame.Board, Int) = parserProlog.createGame(variantTawlbwrdd.toString().toLowerCase)

  var snapBrandubh: GameSnapshotImpl = GameSnapshotImpl(variantBrandubh, gameBrandubh._1, gameBrandubh._2, gameBrandubh._3, Option.empty, 0, 0)
  var snapHnefatafl: GameSnapshotImpl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)
  var snapTablut: GameSnapshotImpl = GameSnapshotImpl(variantTablut, gameTablut._1, gameTablut._2, gameTablut._3, Option.empty, 0, 0)
  var snapTawlbwrdd: GameSnapshotImpl = GameSnapshotImpl(variantTawlbwrdd, gameTawlbwrdd._1, gameTawlbwrdd._2, gameTawlbwrdd._3, Option.empty, 0, 0)

  test("Test number of possible moves - Brandubh."){
    gameBrandubh = parserProlog.createGame(variantBrandubh.toString().toLowerCase)
    snapBrandubh = GameSnapshotImpl(variantBrandubh, gameBrandubh._1, gameBrandubh._2, gameBrandubh._3, Option.empty, 0, 0)

    assert(moveGenerator.gamePossibleMoves(snapBrandubh).size == 40)
  }



  test("Test  possible moves - Brandubh."){
    val possibleMove_Brandubh = List( Move(Coordinate(1,4),Coordinate(1,5)), Move(Coordinate(1,4),Coordinate(1,6)), Move(Coordinate(1,4),Coordinate(1,3)), Move(Coordinate(1,4),Coordinate(1,2)), Move(Coordinate(2,4),Coordinate(2,5)), Move(Coordinate(2,4),Coordinate(2,6)), Move(Coordinate(2,4),Coordinate(2,7)), Move(Coordinate(2,4),Coordinate(2,3)),  Move(Coordinate(2,4),Coordinate(2,2)), Move(Coordinate(2,4),Coordinate(2,1)), Move(Coordinate(4,1),Coordinate(3,1)), Move(Coordinate(4,1),Coordinate(2,1)), Move(Coordinate(4,1),Coordinate(5,1)), Move(Coordinate(4,1),Coordinate(6,1)), Move(Coordinate(4,2),Coordinate(3,2)), Move(Coordinate(4,2),Coordinate(2,2)), Move(Coordinate(4,2),Coordinate(1,2)), Move(Coordinate(4,2),Coordinate(5,2)), Move(Coordinate(4,2),Coordinate(6,2)), Move(Coordinate(4,2),Coordinate(7,2)), Move(Coordinate(4,6),Coordinate(3,6)), Move(Coordinate(4,6),Coordinate(2,6)), Move(Coordinate(4,6),Coordinate(1,6)), Move(Coordinate(4,6),Coordinate(5,6)), Move(Coordinate(4,6),Coordinate(6,6)), Move(Coordinate(4,6),Coordinate(7,6)), Move(Coordinate(4,7),Coordinate(3,7)), Move (Coordinate(4,7),Coordinate(2,7)), Move(Coordinate(4,7),Coordinate(5,7)), Move (Coordinate(4,7),Coordinate(6,7)), Move(Coordinate(6,4),Coordinate(6,5)), Move(Coordinate(6,4),Coordinate(6,6)), Move(Coordinate(6,4),Coordinate(6,7)), Move(Coordinate(6,4),Coordinate(6,3)), Move(Coordinate(6,4),Coordinate(6,2)), Move(Coordinate(6,4),Coordinate(6,1)), Move(Coordinate(7,4),Coordinate(7,5)), Move(Coordinate(7,4),Coordinate(7,6)), Move(Coordinate(7,4),Coordinate(7,3)), Move(Coordinate(7,4),Coordinate(7,2)))

    gameBrandubh = parserProlog.createGame(variantBrandubh.toString().toLowerCase)
    snapBrandubh = GameSnapshotImpl(variantBrandubh, gameBrandubh._1, gameBrandubh._2, gameBrandubh._3, Option.empty, 0, 0)

    moveGenerator.gamePossibleMoves(snapBrandubh).foreach( move => assert(possibleMove_Brandubh.contains(move)))
  }



  test("Test number of possible moves - Hnefatafl."){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    assert(moveGenerator.gamePossibleMoves(snapHnefatafl).size == 116)
  }


  test("Make moves from (1,4) to (4,4) - Hnefatafl"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl,Move(Coordinate(1,4), Coordinate(4,4)))
    assert(step1.getBoard.getCell(Coordinate(4,4)).getPiece.equals( Piece.BlackPawn) )
  }

  test( "Vertical white capture test - Tablut "){
    gameTablut = parserProlog.createGame(variantTablut.toString().toLowerCase)
    snapTablut = GameSnapshotImpl(variantTablut, gameTablut._1, gameTablut._2, gameTablut._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTablut, Move(Coordinate(9,6), Coordinate(6,6)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,3), Coordinate(5,3)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(1,6), Coordinate(4,6)))

    assert(step3.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty))
    assert(step3.getNumberCapturedBlacks == 1)
  }

  test( "Vertical black capture test - Tablut"){
    gameTablut = parserProlog.createGame(variantTablut.toString().toLowerCase)
    snapTablut = GameSnapshotImpl(variantTablut, gameTablut._1, gameTablut._2, gameTablut._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTablut, Move(Coordinate(1,6), Coordinate(4,6)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(3,5), Coordinate(3,6)))

    assert(step2.getBoard.getCell(Coordinate(4,6)).getPiece.equals(Piece.Empty))
    assert(step2.getNumberCapturedWhites == 1)
  }

  test( "Double white capture test - Tablut"){
    gameTablut = parserProlog.createGame(variantTablut.toString().toLowerCase)
    snapTablut = GameSnapshotImpl(variantTablut, gameTablut._1, gameTablut._2, gameTablut._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTablut, Move(Coordinate(9,4), Coordinate(6,4)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(7,5), Coordinate(7,6)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(4,9), Coordinate(4,6)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(5,7), Coordinate(2,5)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(6,9), Coordinate(6,6)))

    assert(step5.getBoard.getCell(Coordinate(6,5)).getPiece.equals(Piece.Empty) &&
      step5.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty))
    println( "Black " + step5.getNumberCapturedBlacks + " White " + step5.getNumberCapturedWhites)
    assert(step5.getNumberCapturedBlacks == 2)
  }

  test( "Triple black capture test - Tawlbwrdd"){
    gameTawlbwrdd = parserProlog.createGame(variantTawlbwrdd.toString().toLowerCase)
    snapTawlbwrdd = GameSnapshotImpl(variantTawlbwrdd, gameTawlbwrdd._1, gameTawlbwrdd._2, gameTawlbwrdd._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTawlbwrdd, Move(Coordinate(1,5), Coordinate(4,4)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(1,6), Coordinate(4,8)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(4,6), Coordinate(4,5)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(6,6), Coordinate(1,2)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(5,7), Coordinate(4,7)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(11,5), Coordinate(11,4)))
    val step7 = moveGenerator.makeMove(step6, Move(Coordinate(3,6), Coordinate(4,6)))

    assert(step7.getBoard.getCell(Coordinate(4,5)).getPiece.equals(Piece.Empty) &&
      step7.getBoard.getCell(Coordinate(4,7)).getPiece.equals(Piece.Empty) &&
      step7.getBoard.getCell(Coordinate(5,6)).getPiece.equals(Piece.Empty))
    assert(step7.getNumberCapturedBlacks == 3)
  }

  test("1. Test white win in (1,1)"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(1,4), Coordinate(1,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,6), Coordinate(1,1)))

    assert(step2.getWinner.equals(Player.White))
  }

  test("2. Test white win in (1,11)"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(1,4), Coordinate(1,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,6), Coordinate(1,11)))

    assert(step2.getWinner.equals(Player.White))
  }

  test("3. Test white win in (11,11)"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(1,4), Coordinate(1,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,6), Coordinate(11,11)))

    println(step2.getWinner)
    assert(step2.getWinner.equals(Player.White))
  }

  test("4. Test white win in (11,1)"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(1,4), Coordinate(1,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,6), Coordinate(11,1)))

    assert(step2.getWinner.equals(Player.White))
  }


  test("Test board king captured 3 sides and throne"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(4,1), Coordinate(4,2)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(5,5), Coordinate(2,5)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(6,2), Coordinate(6,3)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(6,4), Coordinate(10,4)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(8,1), Coordinate(8,4)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(7,5), Coordinate(10,5)))
    val step7 = moveGenerator.makeMove(step6, Move(Coordinate(6,3), Coordinate(6,4)))
    val step8 = moveGenerator.makeMove(step7, Move(Coordinate(6,5), Coordinate(3,5)))
    val step9 = moveGenerator.makeMove(step8, Move(Coordinate(8,4), Coordinate(7,4)))
    val step10 = moveGenerator.makeMove(step9, Move(Coordinate(6,6), Coordinate(6,5)))
    val step11 = moveGenerator.makeMove(step10, Move(Coordinate(7,4), Coordinate(7,5)))
    val step12 = moveGenerator.makeMove(step11, Move(Coordinate(2,5), Coordinate(2,4)))
    val step13 = moveGenerator.makeMove(step12, Move(Coordinate(5,1), Coordinate(5,5)))

    assert(step13.getWinner.equals(Player.Black))
  }

  test("Test board king not captured 3 sides"){
    gameTawlbwrdd = parserProlog.createGame(variantTawlbwrdd.toString().toLowerCase)
    snapTawlbwrdd = GameSnapshotImpl(variantTawlbwrdd, gameTawlbwrdd._1, gameTawlbwrdd._2, gameTawlbwrdd._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTawlbwrdd, Move(Coordinate(1,5), Coordinate(2,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(6,6), Coordinate(3,3)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(1, 6), Coordinate(3, 2)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(1,6), Coordinate(3,2)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(4,6), Coordinate(4,11)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(1,7), Coordinate(4,3)))

    assert(step6.getWinner.equals(Player.None))
  }


  test("Test board 11 king not captured on edge"){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(4,1), Coordinate(2,1)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(5,5), Coordinate(2,5)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(5,1), Coordinate(3,1)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(6,4), Coordinate(2,4)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(6,2), Coordinate(2,2)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(6,5), Coordinate(3,5)))
    val step7 = moveGenerator.makeMove(step6, Move(Coordinate(6,1), Coordinate(5,1)))
    val step8 = moveGenerator.makeMove(step7, Move(Coordinate(6,6), Coordinate(6,1)))
    val step9 = moveGenerator.makeMove(step8, Move(Coordinate(3,2), Coordinate(6,2)))

    assert(step9.getWinner.equals(Player.None))
  }

  /*testBoard11KingOnThroneCapture :-
		newGame(hnefatafl, G),
		makeMove(G, p(1, 4), p(5, 6), NC1, G1),
		makeMove(G1, p(1, 5), p(6, 5), NC2, G2),
		makeMove(G2, p(1, 6), p(6, 7), NC3, G3),
		makeMove(G3, p(4, 6), p(4, 2), NC4, G4),
		makeMove(G4, p(1, 7), p(7, 6), NC5, O),
		gameWinner(O, b)*/

  test("Test board 11 king on throne capture "){
    gameHnefatafl = parserProlog.createGame(variantHnefatafl.toString().toLowerCase)
    snapHnefatafl = GameSnapshotImpl(variantHnefatafl, gameHnefatafl._1, gameHnefatafl._2, gameHnefatafl._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapHnefatafl, Move(Coordinate(1,4), Coordinate(1,3)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(4,6), Coordinate(4,8)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(4,1), Coordinate(4,2)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(5,5), Coordinate(2,5)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(5,1), Coordinate(3,1)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(6,4), Coordinate(1,4)))
    val step7 = moveGenerator.makeMove(step6, Move(Coordinate(2,6), Coordinate(3,6)))
    val step8 = moveGenerator.makeMove(step7, Move(Coordinate(6,5), Coordinate(3,5)))
    val step9 = moveGenerator.makeMove(step8, Move(Coordinate(6,2), Coordinate(6,5)))
    val step10 = moveGenerator.makeMove(step9, Move(Coordinate(5,6), Coordinate(5,1)))
    val step11 = moveGenerator.makeMove(step10, Move(Coordinate(3,6), Coordinate(5,6)))
    val step12 = moveGenerator.makeMove(step11, Move(Coordinate(6,8), Coordinate(10,8)))
    val step13 = moveGenerator.makeMove(step12, Move(Coordinate(6,10), Coordinate(6,9)))
    val step14 = moveGenerator.makeMove(step13, Move(Coordinate(5,7), Coordinate(2,7)))
    val step15 = moveGenerator.makeMove(step14, Move(Coordinate(10,6), Coordinate(9,6)))
    val step16 = moveGenerator.makeMove(step15, Move(Coordinate(6,7), Coordinate(3,7)))
    val step17 = moveGenerator.makeMove(step16, Move(Coordinate(6,9), Coordinate(6,7)))
    val step18 = moveGenerator.makeMove(step17, Move(Coordinate(8,6), Coordinate(8,10)))
    val step19 = moveGenerator.makeMove(step18, Move(Coordinate(8,11), Coordinate(9,11)))
    val step20 = moveGenerator.makeMove(step19, Move(Coordinate(7,7), Coordinate(7,10)))
    val step21 = moveGenerator.makeMove(step20, Move(Coordinate(8,1), Coordinate(9,1)))
    val step22 = moveGenerator.makeMove(step21, Move(Coordinate(7,5), Coordinate(7,2)))
    val step23 = moveGenerator.makeMove(step22, Move(Coordinate(11,4), Coordinate(10,4)))
    val step24 = moveGenerator.makeMove(step23, Move(Coordinate(7,6), Coordinate(7,3)))
    val step25 = moveGenerator.makeMove(step24, Move(Coordinate(9,6), Coordinate(7,6)))

    assert(step25.getWinner.equals(Player.Black) && step25.getBoard.getCell(Coordinate(6,6)).getPiece.equals(Piece.WhiteKing))

  }

  //TODO jumped testBoard11KingFarFromThroneCapture

  test("Test board 9 king on throne capture "){
    gameTablut = parserProlog.createGame(variantTablut.toString().toLowerCase)
    snapTablut = GameSnapshotImpl(variantTablut, gameTablut._1, gameTablut._2, gameTablut._3, Option.empty, 0, 0)

    val step1 = moveGenerator.makeMove(snapTablut, Move(Coordinate(5,2), Coordinate(2,2)))
    val step2 = moveGenerator.makeMove(step1, Move(Coordinate(5,4), Coordinate(2,4)))
    val step3 = moveGenerator.makeMove(step2, Move(Coordinate(4,9), Coordinate(3,9)))
    val step4 = moveGenerator.makeMove(step3, Move(Coordinate(5,3), Coordinate(1,3)))
    val step5 = moveGenerator.makeMove(step4, Move(Coordinate(6,9), Coordinate(7,9)))
    val step6 = moveGenerator.makeMove(step5, Move(Coordinate(3,5), Coordinate(3,8)))
    val step7 = moveGenerator.makeMove(step6, Move(Coordinate(5,9), Coordinate(4,9)))
    val step8 = moveGenerator.makeMove(step7, Move(Coordinate(5,6), Coordinate(3,6)))
    val step9 = moveGenerator.makeMove(step8, Move(Coordinate(9,6), Coordinate(9,7)))
    val step10 = moveGenerator.makeMove(step9, Move(Coordinate(4,5), Coordinate(4,7)))
    val step11 = moveGenerator.makeMove(step10, Move(Coordinate(2,5), Coordinate(4,5)))
    val step12 = moveGenerator.makeMove(step11, Move(Coordinate(6,5), Coordinate(6,9)))
    assert(step12.getBoard.getCell(Coordinate(7,5)).getPiece.equals(Piece.WhitePawn))

    val step13 = moveGenerator.makeMove(step12, Move(Coordinate(6,1), Coordinate(6,5)))
    assert(step13.getBoard.getCell(Coordinate(7,5)).getPiece.equals(Piece.Empty))

    assert(step13.getNumberCapturedWhites == 1)

    val step14 = moveGenerator.makeMove(step13, Move(Coordinate(5,7), Coordinate(8,7)))
    val step15 = moveGenerator.makeMove(step14, Move(Coordinate(5,8), Coordinate(5,6)))
    val step16 = moveGenerator.makeMove(step15, Move(Coordinate(4,7), Coordinate(1,7)))
    val step17 = moveGenerator.makeMove(step16, Move(Coordinate(5,1), Coordinate(5,4)))


    assert(step17.getWinner.equals(Player.Black) && step17.getBoard.getCell(Coordinate(5,5)).getPiece.equals(Piece.WhiteKing))

  }



}
