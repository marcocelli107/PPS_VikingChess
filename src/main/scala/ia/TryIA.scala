package ia

import model.{GameVariant, ParserProlog, ParserPrologImpl, TheoryGame}

object TryIA extends App {
   val THEORY: String = TheoryGame.GameRules.toString
   val parserProlog:ParserProlog= ParserPrologImpl(THEORY)
   val parserPrologCopy: ParserProlog= parserProlog.copy()
   val board = parserPrologCopy.createGame(GameVariant.Tablut.nameVariant)._3

   val ev: EvaluationFunctionImpl = new EvaluationFunctionImpl
   println( board)
   println( ev.scoreKingNearCorners(board))


   /*def makePawn(board: Board, startCoord: Pair[Int], endCoord: Pair[Int] ): Board= {
     def setPawn(cellsInput: Seq[BoardCell], cellsOutput: Seq[BoardCell], bordCell: BoardCell):Seq[BoardCell]= cellsInput match {
        case h::t if (h.getCoordinate.equals(bordCell.getCoordinate)) => cellsOutput ++ bordCell::t
     }
   }*/

}
