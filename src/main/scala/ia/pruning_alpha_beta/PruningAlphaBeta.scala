package ia.pruning_alpha_beta

import ia.evaluation_function.EvaluationFunction
import model.game.Level.Level
import model.game.MaxMin.MaxMin
import model.game.Player.Player
import model.game.{GameSnapshot, MaxMin, Move, MoveGenerator, Player}

sealed trait MiniMax {

  def findBestMove(gameSnapshot: GameSnapshot): Move

}

object MiniMaxImpl {
  def apply(levelIA: Level): MiniMaxImpl = new MiniMaxImpl(levelIA)
}

class MiniMaxImpl(levelIA: Level) extends MiniMax {

    val evaluationFunction = new EvaluationFunction

    override def findBestMove(gameSnapshot: GameSnapshot): Move = {

      @scala.annotation.tailrec
      def _findBestMove(bestMove: Option[Move], bestScore: Int, gamePossibleMove: List[Move]): Move = gamePossibleMove match {
        case Nil => bestMove.get
        case h :: t =>
          val sonGameSnapshot = MoveGenerator.makeMove(gameSnapshot.getCopy, h)
          val moveScore = pruningAlfaBeta(sonGameSnapshot, levelIA.depth - 1 , Int.MinValue, Int.MaxValue, getMinOrMaxPhase(gameSnapshot.getPlayerToMove))
          val (newBestMove, newBestScore) = chooseTheBest(h,bestMove,bestScore,moveScore,gameSnapshot.getPlayerToMove)
          _findBestMove(newBestMove , newBestScore, t)
      }

      _findBestMove(Option.empty, Int.MinValue, MoveGenerator.gamePossibleMoves(gameSnapshot))
    }

    private def chooseTheBest(move: Move, bestCoord: Option[Move], bestScore:Int, newScore:Int, iaPlayer: Player):(Option[Move],Int) =  iaPlayer match {
      case Player.White if (newScore > bestScore && bestCoord.nonEmpty) => (Option(move), newScore)
      case Player.Black if (newScore < bestScore && bestCoord.nonEmpty) => (Option(move), newScore)
      case _  if bestCoord.isEmpty=> (Option(move),newScore )
      case _ => (bestCoord,bestScore)
    }

    private def getMinOrMaxPhase(iaPlayer: Player): MaxMin = iaPlayer match {
      case Player.Black => MaxMin.min
      case _ => MaxMin.Max
    }


  def pruningAlfaBeta(sonGameSnapshot: GameSnapshot, depth: Int, alfa: Int, beta: Int, phase: MaxMin): Int = (depth, phase) match {
    case (_, _) if isTerminalNode(sonGameSnapshot) => evaluationFunction.score(sonGameSnapshot, levelIA)
    case (0, _) => evaluationFunction.score(sonGameSnapshot, levelIA)
    case (_, MaxMin.Max) => maximizationPhase(sonGameSnapshot, depth, alfa, beta)
    case _ => minimizationPhase(sonGameSnapshot, depth, alfa, beta)
  }

  def maximizationPhase(sonGameSnapshot: GameSnapshot, depth: Int, alfa: Int, beta: Int): Int = {
    val tempVal: Int = Int.MinValue
    val gameMoves: List[Move] = MoveGenerator.gamePossibleMoves(sonGameSnapshot)

    @scala.annotation.tailrec
    def _maximizationPhase(gameMoves: List[Move], tempVal: Int, depth: Int, alfa: Int, beta: Int): Int = gameMoves match {
      case Nil => tempVal
      case _ if beta <= alfa => tempVal
      case h :: t =>
        val sonGame: GameSnapshot = MoveGenerator.makeMove(sonGameSnapshot.getCopy, h)
        val newTempVal = math.max(tempVal, pruningAlfaBeta(sonGame, depth - 1, alfa, beta, MaxMin.min))
        val newAlfa = math.max(alfa, newTempVal)
        _maximizationPhase(t, newTempVal, depth, newAlfa, beta)
    }

    _maximizationPhase(gameMoves, tempVal, depth, alfa, beta)
  }

  def minimizationPhase(sonGameSnapshot: GameSnapshot, depth: Int, alfa: Int, beta: Int): Int = {
    val tempVal: Int = Int.MaxValue
    val gameMoves: List[Move] = MoveGenerator.gamePossibleMoves(sonGameSnapshot)


    @scala.annotation.tailrec
    def _minimizationPhase(gameMoves: List[Move], tempVal: Int, depth: Int, alfa: Int, beta: Int): Int = gameMoves match {
      case Nil => tempVal
      case _ if beta <= alfa => tempVal
      case h :: t =>
        val sonGame: GameSnapshot = MoveGenerator.makeMove(sonGameSnapshot.getCopy, h)
        val newTempVal = math.min(tempVal, pruningAlfaBeta(sonGame, depth - 1, alfa, beta, MaxMin.Max))
        val newBeta = math.min(beta, newTempVal)
        _minimizationPhase(t, newTempVal, depth, alfa, newBeta)

    }

    _minimizationPhase(gameMoves, tempVal, depth, alfa, beta)
  }

  def isTerminalNode(gameSnapshot: GameSnapshot): Boolean = !gameSnapshot.getWinner.equals(Player.None)
}