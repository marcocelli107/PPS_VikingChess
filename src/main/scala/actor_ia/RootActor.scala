package actor_ia

import akka.actor.ActorRef
import utils.Move

import scala.collection.mutable

object RootActor {

  case class MaxRootActor() extends MaxActor() {

    override def updateBestMove(hashMapSonRef: mutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef: mutable.HashMap[ActorRef, Move], sonRef: ActorRef)

  }

  case class MinRootActor() extends MinActor() {

    override def updateBestMove(hashMapSonRef: mutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef: mutable.HashMap[ActorRef, Move], sonRef: ActorRef)

  }

  def rootUpdateBestMove(hashMapSonRef: mutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option(hashMapSonRef(sonRef))
}