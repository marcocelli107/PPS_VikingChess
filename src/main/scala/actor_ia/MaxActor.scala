package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

class MaxActor() extends MiniMaxActor() {

  def initAlfa: Int = Int.MinValue

  override def createChild(): Props =
    Props(new MinActor())

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    //alfa = scala.math.max(alfa, score)
    score > alfa
  }
}
