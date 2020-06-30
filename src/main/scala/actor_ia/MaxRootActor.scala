package actor_ia

import akka.actor.{ActorRef, Props}
import model.GameSnapshot
import utils.Move

import scala.collection.immutable.HashMap
import scala.collection.mutable


case class MaxRootActor() {

  private var hashMapSonRef: mutable.HashMap[ActorRef, Move] = mutable.HashMap.empty
  private var bestChildMove: Move = _


  //override def sonCreated(sonActor: ActorRef, sonMove: Move): Unit = hashMapSonRef += (sonActor -> sonMove)


  //override def updateBestMove: Unit = bestChildMove = hashMapSonRef(context.sender())



}
