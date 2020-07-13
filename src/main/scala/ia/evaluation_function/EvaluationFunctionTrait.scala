package ia.evaluation_function

import model.game.Level.Level

trait EvaluationFunctionTrait {

  /** Returns final score.
   *
   * @param levelIA
   * difficulty chosen.
   * @return total score int
   */
  def score(levelIA: Level): Int

  /** Checks if the king is near a corner.
   *
   * @return boolean
   */
  def kingAdjacentToCorner: Boolean

  /** Checks if the king can move near a corner.
   *
   * @return boolean
   */
  def kingNearCornerInOne: Boolean

  /** Checks if the king can move to a corner.
   *
   * @return boolean
   */
  def kingToCornerInOne: Boolean

  /** Checks if the king is catchable.
   *
   * @return boolean
   */
  def kingCapturedInOne: Boolean

  /** Returns score for a wrong barricade.
   *
   * @return a FactionsScore for white and black scores.
   */
  def scoreWrongBarricade: FactionsScore

  /** Returns score for last pawn moved catchable in one.
   *
   * @return Int: score capture last pawn moved
   */
  def scoreLastPawnMovedCatchableInOne: Int

  /** Returns score according to distance from throne.
   *
   * @return Int: scoreKingOnThrone
   */
  def scoreKingOnThrone: Int

  /** Returns score for a white tower.
   *
   * @return Int: scoreTower
   */
  def scoreTower: Int

  /** Returns score for a captured black piece.
   *
   * @return Int: scoreCapture
   */
  def scoreCapturedBlack: Int

  /** Returns score for the row and column controlled from king.
   *
   * @return Int: score row, column owned
   */
  def scoreKingIsInFreeRowOrColumn: Int

  /** Returns score for a captured white piece.
   *
   * @return Int: score for captures
   */
  def scoreCapturedWhite: Int

  /** Returns score according to number of blacks surround to king.
   *
   * @return Int: score for surrounding pieces
   */
  def scoreBlackSurroundTheKing: Int

  /** Returns score according to number of blacks on king's diagonals.
   *
   * @return score int
   */
  def scoreBlackOnKingDiagonal: Int

  /** Returns score for a black cordon.
   *
   * @return score int
   */
  def scoreBlackCordon: Int

  /** Returns score according to king possible moves.
   *
   * @return score int
   */
  def scoreMovesKing: Int

}

case class FactionsScore(whiteScore: Int, blackScore: Int)