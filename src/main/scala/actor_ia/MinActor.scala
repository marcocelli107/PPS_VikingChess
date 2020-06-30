package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

class MinActor() extends MiniMaxActor() {

  def initAlfa: Int = Int.MaxValue

  override def createChild(): Props =
    Props(new MaxActor())

  override def miniMaxComparison(score: Int, alfa: Int): Boolean = {
    //alfa = scala.math.min(alfa, score)
    score < alfa
  }
}
