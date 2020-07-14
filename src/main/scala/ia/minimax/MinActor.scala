package ia.minimax

import akka.actor.Props
import model.game.Level.Level

class MinActor(levelIA: Level) extends MiniMaxActor(levelIA) {

  override def initAlpha: Int = Int.MaxValue

  override def createChild(): Props =
    Props(new MaxActor(levelIA))

  override def miniMaxComparison(score: Int, alpha: Int): Boolean = {
    score < alpha
  }
}
