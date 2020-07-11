package ia.minimax

import akka.actor.Props
import model.game.Level.Level

class MaxActor(levelIA: Level) extends MiniMaxActor(levelIA) {

  override def initAlpha: Int = Int.MinValue

  override def createChild(): Props =
    Props(new MinActor(levelIA))

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    score > alfa
  }
}
