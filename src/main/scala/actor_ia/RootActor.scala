package actor_ia

import akka.actor.ActorRef
import model.Level
import utils.Move

import scala.collection.immutable

object RootActor {

  case class MaxRootActor(levelIA: Level.Val) extends MaxActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  case class MinRootActor(levelIA: Level.Val) extends MinActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  def rootUpdateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option(hashMapSonRef(sonRef))
}