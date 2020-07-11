package ia.minimax

import akka.actor.ActorRef
import model.game.Level.Level
import model.game.Move

import scala.collection.immutable

object RootActor {

  /** Represents maximizing root
    *
    * @param levelIA
    *                difficulty chosen by user
    */
  case class MaxRootActor(levelIA: Level) extends MaxActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  /** Represents minimizing root
    *
    * @param levelIA
    *                difficulty chosen by user
    */
  case class MinRootActor(levelIA: Level) extends MinActor(levelIA) {

    override def updateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
      rootUpdateBestMove(hashMapSonRef, sonRef)

  }

  /** Updates best move
    *
    * @param hashMapSonRef
    *                     Map of generated children
    *
    * @return Option[Move]
    *
    */
  def rootUpdateBestMove(hashMapSonRef: immutable.HashMap[ActorRef, Move], sonRef: ActorRef): Option[Move] =
    Option(hashMapSonRef(sonRef))
}
