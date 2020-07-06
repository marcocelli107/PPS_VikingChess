package actor_ia

import akka.actor.Props
import model.Level

class MinActor(levelIA: Level.Val) extends MiniMaxActor(levelIA) {

  override def initAlfa: Int = Int.MaxValue

  override def createChild(): Props =
    Props(new MaxActor(levelIA))

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    score < alfa
  }

}
