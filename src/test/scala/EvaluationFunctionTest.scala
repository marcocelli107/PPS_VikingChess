import model.game.Player.Player
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import model.game.BoardGame.Board
import ia.evaluation_function.{EvaluationFunction, ScoreProvider}
import model.game.{Coordinate, GameSnapshot, GameVariant, Level, Move, MoveGenerator}
import model.prolog.ParserProlog

@RunWith(classOf[JUnitRunner])
class EvaluationFunctionTest extends FunSuite {

  var snapshot: GameSnapshot = _
  var game: (Player, Player, Board, Int) = _

  test("Tests score king near corner - Brandubh"){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5),Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7),Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4),Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6),Coordinate(1,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,4),Coordinate(7,5)))

    assert(EvaluationFunction(snapshot).score(Level.Newcomer) == ScoreProvider.KingEscapeToCorner)
  }

  test("Tests score king to corner in one - Brandubh"){
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6),Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5),Coordinate(1,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,7),Coordinate(2,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4),Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6),Coordinate(4,6)))

    assert(EvaluationFunction(snapshot).score(Level.Newcomer) == ScoreProvider.KingEscapeToCorner)
  }

  test("Tests score capture king in one on throne - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).score(Level.Standard) == -ScoreProvider.KingCatchableInOne)
  }

  test("Tests score capture king near throne - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,3), Coordinate(1,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,1), Coordinate(5,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,4), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,4), Coordinate(6,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(5,4)))

    assert(EvaluationFunction(snapshot).score(Level.Standard) == -ScoreProvider.KingCatchableInOne)
  }

  test("Test score capture king far from throne - Brandubh") {
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(6,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,4), Coordinate(4,2)))

    assert(EvaluationFunction(snapshot).score(Level.Standard) == -ScoreProvider.KingCatchableInOne)
  }

  test("Test score capture king far from throne - Tawlbwurdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(9,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,2), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,3), Coordinate(9,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,2), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(10,6)))

    assert(EvaluationFunction(snapshot).score(Level.Standard) == -ScoreProvider.KingCatchableInOne)
  }

  test("Test initial score is draw - Brandubh") {
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    assert(EvaluationFunction(snapshot).score(Level.Newcomer) == ScoreProvider.Draw)
  }

  test("Test score black ahead - Brandubh") {
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).score(Level.Newcomer) < -100)
  }

  test("Test score white ahead with rows and columns owned - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).score(Level.Standard) > 50)
  }

  test("Test score black surround king on three side - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,4), Coordinate(3,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(4,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,5), Coordinate(6,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,3), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(9,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,6), Coordinate(6,2)))

    assert(EvaluationFunction(snapshot).score(Level.Newcomer) < -50)
  }

  test("Test score captured pieces (4 whites / 2 blacks) - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreCapturedBlack == 40 &&
      EvaluationFunction(snapshot).scoreCapturedWhite == 200)
  }

  test("Test score white towers near throne - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    val numberOfTowers = 4

    assert(EvaluationFunction(snapshot).scoreTower == ScoreProvider.TowerCoefficient * numberOfTowers)
  }

  test("Test score white king on throne - Tablut") {
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    assert(EvaluationFunction(snapshot).scoreKingOnThrone == ScoreProvider.KingOnThrone)
  }

  test("Test score white king far from throne - Tablut") {
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
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

    val quadraticDistanceFromKingToThrone = 10

    assert(EvaluationFunction(snapshot).scoreKingOnThrone == ScoreProvider.KingDistanceToCornerDividend / quadraticDistanceFromKingToThrone)
  }

  test("Test score right barricade on 3 sides and wrong circle cordon - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon - ScoreProvider.WrongCordon)
  }

  test("Test score circle cordon on all sides - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon +
      + whitePawnsInCordon * ScoreProvider.WhiteInsideCordon)
  }

  test("Test score right circle cordon - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon +
    + whitePawnsInCordon * ScoreProvider.WhiteInsideCordon)
  }

  test("Test score wrong circle cordon - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon - ScoreProvider.WrongCordon)
  }

  test("Test score right diagonal barricade - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,2), Coordinate(2,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(3,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(3,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,4), Coordinate(1,3)))

    val blackPawnsInCordon: Int = 3

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon)
  }

  test("Test score right vertical barricade - Tablut") {
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
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

    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon)
  }

  test("Test score right horizontal barricade and wrong circle cordon - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
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

    println(snapshot.getBoard.consoleRepresentation)
    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon + ScoreProvider.RightCordon - ScoreProvider.WrongCordon)
  }

  test("Tests score wrong barricade (2 black, 1 white) - Tawlbwrdd"){
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,5), Coordinate(1,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(2,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,1), Coordinate(10,1)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,8), Coordinate(1,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,2), Coordinate(11,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(1,8), Coordinate(1,10)))

    val wrongBarricade = EvaluationFunction(snapshot).scoreWrongBarricade

    assert(wrongBarricade._2 == 4 * ScoreProvider.WrongBarricade && wrongBarricade._1 == 2 * ScoreProvider.WrongBarricade)
  }

  test("Tests score two black on king's diagonal - Tablut"){
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
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

    val blackOnDiagonalScore = EvaluationFunction(snapshot).scoreBlackOnKingDiagonal

    assert(blackOnDiagonalScore == ScoreProvider.BlackOnDiagonalKing * 2)
  }

  test("Test score last black moved catchable in one - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(3,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(3,5)))


    assert(EvaluationFunction(snapshot).scoreLastPawnMovedCatchableInOne == ScoreProvider.LastBlackMovedCatchableInOne)
  }

  test("Test score last white moved catchable in one - Brandubh") {
    game = ParserProlog.createGame(GameVariant.Brandubh.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Brandubh, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,2), Coordinate(5,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,3), Coordinate(2,3)))

    assert(EvaluationFunction(snapshot).scoreLastPawnMovedCatchableInOne == ScoreProvider.LastWhiteMovedCatchableInOne)
  }
  
  test("Test score for all difficulties - Hnefatafl") {
    game = ParserProlog.createGame(GameVariant.Hnefatafl.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Hnefatafl, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,1), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(4,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,6), Coordinate(4,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,8), Coordinate(4,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,8), Coordinate(5,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,11), Coordinate(2,11)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,6), Coordinate(5,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,7), Coordinate(5,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,10), Coordinate(1,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,9), Coordinate(9,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,1), Coordinate(8,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(8,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,6), Coordinate(10,4)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,6), Coordinate(8,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,4), Coordinate(11,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,7), Coordinate(8,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,3), Coordinate(9,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,9), Coordinate(9,10)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(11,8), Coordinate(10,8)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(9,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(10,8), Coordinate(10,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,7), Coordinate(9,9)))


    assert(EvaluationFunction(snapshot).computeNewcomerScore == -100 &&
      EvaluationFunction(snapshot).computeStandardScore == -100 &&
      EvaluationFunction(snapshot).computeAdvancedScore == -100)
  }

  test("Test score for all difficulties - Tablut") {
    game = ParserProlog.createGame(GameVariant.Tablut.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tablut, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,2), Coordinate(3,2)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,5), Coordinate(3,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(2,5), Coordinate(2,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,5), Coordinate(4,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,5), Coordinate(7,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(8,5), Coordinate(8,3)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(5,5), Coordinate(2,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(9,6), Coordinate(7,6)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,7), Coordinate(6,7)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(7,6), Coordinate(7,2)))

    assert(EvaluationFunction(snapshot).computeNewcomerScore == ScoreProvider.KingEscapeNearCorner &&
      EvaluationFunction(snapshot).computeStandardScore == ScoreProvider.KingEscapeNearCorner &&
      EvaluationFunction(snapshot).computeAdvancedScore == ScoreProvider.KingEscapeNearCorner)
  }

  test("Test score barricade on up and right sides and wrong double circle cordon - Tawlbwrdd") {
    game = ParserProlog.createGame(GameVariant.Tawlbwrdd.toString.toLowerCase)
    snapshot = GameSnapshot(GameVariant.Tawlbwrdd, game._1, game._2, game._3, Option.empty, 0, 0)

    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(6,9), Coordinate(4,9)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(4,6), Coordinate(4,5)))
    snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(3,6), Coordinate(3,8)))

    val blackPawnsInCordon = 8 + 4 + 4

    println(snapshot.getBoard.consoleRepresentation)
    println(EvaluationFunction(snapshot).scoreBlackCordon)
    assert(EvaluationFunction(snapshot).scoreBlackCordon == blackPawnsInCordon * ScoreProvider.PawnInCordon
      + ScoreProvider.RightCordon
      - ScoreProvider.WrongCordon - ScoreProvider.WrongCordon)
  }
}