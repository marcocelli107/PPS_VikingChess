import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Theory}
import model.prolog.ParserProlog
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PossibleMovesTest extends FunSuite {
  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory(getClass.getResourceAsStream(ParserProlog.THEORY))
  var goal: SolveInfo = _
  prolog.setTheory(theory)

  test("Tests if a new Brandubh game has 40 possible moves.") {
    goal = prolog.solve("testGamePossibleMoves.")
    assert(goal.isSuccess)
  }

  test("Tests if a new Hnefatafl game has 116 possible moves.") {
    goal = prolog.solve("testGamePossibleMoves1.")
    assert(goal.isSuccess)
  }

  test("Tests if the possible moves for pawn in coordinate (1,4) in a new Tablut game are correct.") {
    goal = prolog.solve("testGetCoordPossibleMoves.")
    assert(goal.isSuccess)
  }

  test("Tests if the possible moves list for an empty cell is empty.") {
    goal = prolog.solve("testGetCoordPossibleMovesEmptyCell.")
    assert(goal.isSuccess)
  }

  test("Tests if the possible moves list for a non player to move cell is empty.") {
    goal = prolog.solve("testGetCoordPossibleMovesNotPlayerToMove.")
    assert(goal.isSuccess)
  }

  test("Tests if the possible moves list for an blocked piece is empty.") {
    goal = prolog.solve("testGetCoordPossibleMovesBlockedPiece.")
    assert(goal.isSuccess)
  }

  test("Tests if the king can move to special cells.") {
    goal = prolog.solve("testGetCoordPossibleMovesKingSpecialCells.")
    assert(goal.isSuccess)
  }
}
