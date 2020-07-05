import actor_ia.{MoveGenerator, ScoreProvider}
import ia.EvaluationFunction
import model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import utils.BoardGame.Board
import utils.{Coordinate, Move}

@RunWith(classOf[JUnitRunner])
class EvaluationFunctionTest extends FunSuite {

  val THEORY: String = TheoryGame.GameRules.toString
  val prolog: ParserProlog = ParserPrologImpl(THEORY)
  var snapshot: GameSnapshot = _
  var game: (Player.Val, Player.Val, Board, Int) = _

  test("Tests Score King Near Corner - Brandubh."){
    game = prolog.createGame(GameVariant.Brandubh.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5),Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7),Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4),Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6),Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(7,6)))

    assert(EvaluationFunction.score(snapshot).equals(900))
  }

  test("Tests Score King To Corner In One - Brandubh."){
    game = prolog.createGame(GameVariant.Brandubh.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5),Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7),Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4),Coordinate(4,7)))

    assert(EvaluationFunction.score(snapshot).equals(900))
  }

  test("Tests Score Capture King In One On Throne - Tablut."){
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(1,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,9), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(2,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,8)))


    assert(EvaluationFunction.score(snapshot) == -900)
  }

  test("Tests Score Capture King Near Throne - Tablut."){
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,4), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,4)))

    assert(EvaluationFunction.score(snapshot) == -900)
  }

  test("Test Score Capture King Far From Throne - Brandubh") {
    game = prolog.createGame(GameVariant.Brandubh.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(6,4)))

    assert(EvaluationFunction.score(snapshot) == -900)
  }

  test("Test Score Capture King Far From Throne - Tawlbwurdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,2), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,3), Coordinate(9,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,2), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(10,6)))

    assert(EvaluationFunction.score(snapshot) == -900)
  }

  test("Test Initial Score Is Better For Blacks") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    assert(EvaluationFunction.score(snapshot) < 0)
  }

  test("Test Score Black Ahead - Brandubh") {
    game = prolog.createGame(GameVariant.Brandubh.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(3,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(4,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,1), Coordinate(3,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(3,5)))

    assert(EvaluationFunction.score(snapshot) < -100)
  }

  test("Test Score White Ahead With Rows And Columns Owned - Hnefatafl") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,11), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,2), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(8,2)))

    assert(EvaluationFunction.score(snapshot) > 50)
  }

  test("Test Score Black Surround King On Three Side - Tawlbwrdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(9,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,2)))

    assert(EvaluationFunction.score(snapshot) < -100)
  }

  test("Test Score Captured Pieces (4 whites / 2 blacks) - Hnefatafl") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,2), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,7), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(3,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(3,6)))

    assert(EvaluationFunction.scoreCapturedBlack(snapshot) == 40 &&
      EvaluationFunction.scoreCapturedWhite(snapshot) == 200)
  }

  test("Test Score White Towers Near Throne - Hnefatafl") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    val numberOfTowers = 4
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreTower() == ScoreProvider.TowerCoefficient * numberOfTowers)
  }

  test("Test Score White King On Throne - Tablut") {
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreKingOnThrone() == ScoreProvider.KingOnThroneScore)
  }

  test("Test Score White King Far From Throne - Tablut") {
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,2), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,4), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(6,2)))

    EvaluationFunction.usefulValues(snapshot)

    /** Quadratic Distance from cell(6,3) to center cell(5,5) */
    val quadraticDistanceFromKingToThrone = 10

    assert(EvaluationFunction.scoreKingOnThrone() == ScoreProvider.KingDistanceToCornerDividend / quadraticDistanceFromKingToThrone)
  }




  test("Test Score Right Barricade On 3 Sides and Wrong Circle Cordon - Tawlbwrdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,5), Coordinate(10,4)))

    val blackPawnsInCordon = 14 + 4
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightBarricade - ScoreProvider.WrongCordon)
  }

  test("Test Score Circle Cordon On All Sides - Tawlbwrdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,5), Coordinate(10,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(8,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,7), Coordinate(10,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(8,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(9,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,7), Coordinate(6,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,10), Coordinate(8,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,7), Coordinate(2,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,7), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,11), Coordinate(3,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,11), Coordinate(3,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,7), Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,8), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,8), Coordinate(3,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(6,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,9), Coordinate(4,9)))

    val blackPawnsInCordon: Int = 22
    val whitePawnsInCordon: Int = 13
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightCordon +
      + whitePawnsInCordon * ScoreProvider.WhiteInCordon)
  }

  test("Test Score Right Circle Cordon - Tawlbwrdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,1), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,4), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(8,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,7), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(4,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,2), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,7), Coordinate(1,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,8), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(5,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,11), Coordinate(6,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,7), Coordinate(9,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,10), Coordinate(7,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,10), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,11), Coordinate(9,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,11), Coordinate(9,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,4), Coordinate(8,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,9), Coordinate(8,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,6), Coordinate(10,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,5), Coordinate(11,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,7), Coordinate(11,10)))

    val blackPawnsInCordon: Int = 19
    val whitePawnsInCordon: Int = 12
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightCordon +
    + whitePawnsInCordon * ScoreProvider.WhiteInCordon)
  }

  test("Test Score Wrong Circle Cordon - Hnefatafl") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,4), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(5,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(7,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7), Coordinate(4,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,4), Coordinate(5,4)))

    val blackPawnsInCordon: Int = 10
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn - ScoreProvider.WrongCordon)
  }

  test("Test Score Right Diagonal Barricade - Hnefatafl") {
    game = prolog.createGame(GameVariant.Hnefatafl.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))

    val blackPawnsInCordon: Int = 3
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightBarricade)
  }

  test("Test Score Right Vertical Barricade - Tablut") {
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,1), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,4), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,5), Coordinate(7,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(6,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,1), Coordinate(3,3)))

    val blackPawnsInCordon: Int = 9
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightBarricade)
  }

  test("Test Score Right Horizontal Barricade and Wrong Circle Cordon - Tawlbwrdd") {
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(4,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,7), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(1,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(6,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,7), Coordinate(1,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(8,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(4,8)))

    val blackPawnsInCordon: Int = 13 + 4
    EvaluationFunction.usefulValues(snapshot)

    assert(EvaluationFunction.scoreBlackCordon() == blackPawnsInCordon * ScoreProvider.CordonPawn + ScoreProvider.RightBarricade - ScoreProvider.WrongCordon)
  }

  test("Tests Score Wrong Barricade (2 black, 1 white) - Tawlbwrdd."){
    game = prolog.createGame(GameVariant.Tawlbwrdd.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(1,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(2,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(10,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(1,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(11,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(1,10)))

    EvaluationFunction.usefulValues(snapshot)
    val wrongBarricade = EvaluationFunction.wrongBarricadeScore()

    assert(wrongBarricade._2 == -2 * ScoreProvider.WrongBarricade && wrongBarricade._1 == - ScoreProvider.WrongBarricade)
  }

  test("Tests Score Two Black on King's diagonal - Tablut."){
    game = prolog.createGame(GameVariant.Tablut.toString().toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(2,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,6), Coordinate(2,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(3,4)))

    EvaluationFunction.usefulValues(snapshot)
    val blackOnDiagonalScore = EvaluationFunction.scoreBlackOnKingDiagonal()

    assert(blackOnDiagonalScore == ScoreProvider.BlackDiagonalToKing * 2)
  }

}
