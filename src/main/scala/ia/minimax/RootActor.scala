package ia.minimax

import akka.actor.ActorRef
import model.game.Level.Level
import model.game.Move

import scala.collection.immutable

object RootActor {

  case class MaxRootActor(levelIA: Level) extends MaxActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  case class MinRootActor(levelIA: Level) extends MinActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  def rootUpdateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option(hashMapSonRef(sonRef))
}
