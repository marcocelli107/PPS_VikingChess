import java.io.FileInputStream

import alice.tuprolog.{Prolog, Theory}
import ia.EvaluationFunctionImpl
import model.{Piece, TheoryGame}
import org.scalatest.FunSuite
import utils.BoardGame.BoardCell
import utils.Coordinate

class EvaluationFunctionTest extends FunSuite{

  val prolog: Prolog = new Prolog()
  val theory: Theory = new Theory(new FileInputStream(TheoryGame.GameRules.toString))
  val ef: EvaluationFunctionImpl =  new EvaluationFunctionImpl()

  test("Tests if King is near corners."){
    val c1:Coordinate = Coordinate(1,2)
    val c2:Coordinate = Coordinate(2,1)
    val c3:Coordinate = Coordinate(1,10)
    val c4:Coordinate = Coordinate(11,2)
    val c5:Coordinate = Coordinate(11,10)
    val c6:Coordinate = Coordinate(10,11)
    val c7:Coordinate = Coordinate(6,7)
    val c8:Coordinate = Coordinate(6,6)
    assert(ef.scoreKingNearCorners(11, c1) != 0 )
    assert(ef.scoreKingNearCorners(11, c2) != 0 )
    assert(ef.scoreKingNearCorners(11, c3) != 0 )
    assert(ef.scoreKingNearCorners(11, c4) != 0 )
    assert(ef.scoreKingNearCorners(11, c5) != 0 )
    assert(ef.scoreKingNearCorners(11, c6) != 0 )
    assert(ef.scoreKingNearCorners(7, c7) != 0 )
    assert(ef.scoreKingNearCorners(11, c8) == 0 )
  }

  test("Test Sequence of Free Cells"){
    var seqEmpty: Seq [BoardCell] = Seq()
    for ( i <- 1 to 10 ) {
      seqEmpty = seqEmpty :+ BoardCell( Coordinate(i,3),Piece.Empty )
    }
    val seqWithPiece = seqEmpty:+ BoardCell( Coordinate(11,3), Piece.WhiteKing )
    assert(ef.isSequenceFreeCells(seqEmpty))
    assert(!ef.isSequenceFreeCells(seqWithPiece))
  }



  test("Test if King is in free row or column"){
    var seq1: Seq [BoardCell] = Seq()
    var seq2: Seq [BoardCell] = Seq()
    for ( i <- 1 to 10 ) {
      seq1 = seq1 :+ BoardCell(Coordinate(i,3),Piece.Empty )
      seq2 = seq2 :+ BoardCell(Coordinate(i,3),Piece.BlackPawn )
    }
    val seqKingFreeRoworColumn = seq1 :+ BoardCell(Coordinate(11,3), Piece.WhiteKing )
    val seqKingNotFreeRoworColumn = seq2 :+ BoardCell(Coordinate(11,3), Piece.WhiteKing  )

      assert( ef.scoreKingIsInFreeRowOrColumn(seqKingNotFreeRoworColumn, seqKingNotFreeRoworColumn)  == 0)
      assert( ef.scoreKingIsInFreeRowOrColumn(seqKingNotFreeRoworColumn, seqKingFreeRoworColumn) != 0)
      assert( ef.scoreKingIsInFreeRowOrColumn(seqKingFreeRoworColumn, seqKingNotFreeRoworColumn) != 0)
      assert( ef.scoreKingIsInFreeRowOrColumn(seqKingFreeRoworColumn, seqKingNotFreeRoworColumn) < ef.scoreKingIsInFreeRowOrColumn(seqKingFreeRoworColumn, seqKingFreeRoworColumn))

  }



}
