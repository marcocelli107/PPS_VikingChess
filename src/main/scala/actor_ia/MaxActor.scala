package actor_ia

import akka.actor.Props
import model.Level

class MaxActor(levelIA: Level.Val) extends MiniMaxActor(levelIA) {

  override def initAlfa: Int = Int.MinValue

  override def createChild(): Props =
    Props(new MinActor(levelIA))

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    score > alfa
  }

}
