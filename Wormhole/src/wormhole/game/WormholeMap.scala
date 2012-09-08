package wormhole.game

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import wormhole.actor._
import wormhole.WormholeSystem
/**
 * Author: Brandon
 */
class WormholeMap(val width:Int, val height:Int, val players:List[Player]){
	val ref:ActorRef = WormholeSystem.actorOf(Props(classOf[WormholeMapImpl]))
	
	def player(id:PlayerId) = players find {_.id == id}
	
	def addObject(obj:BaseObject){
		ref ! obj
	}
	def updateAll(){
		ref ! 'Update
	}
	def objectAt(x:Int, y:Int) = {
		fetch ((ref ? ('At, x,y)).mapTo[Option[BaseObject]])
	}
	def objects = fetch ((ref ? 'Objects).mapTo[List[BaseObject]])
}
 
/**
 * Actor backend for WormholeMap.
 * Author: Brandon
 */
private class WormholeMapImpl extends Actor{
	
	var objects:List[BaseObject] = Nil
	
	def receive = {
		case (obj:BaseObject) =>
			objects ::= obj
		case 'Update =>
			objects foreach {_.update()}
		case ('At,x:Int,y:Int) =>
			val zipped = objects zip (objects map {_.dataFuture})
			val res = zipped find {
				tup =>
					val loc = fetch(tup._2).location
					loc.x==x && loc.y == y
			}
			sender ! (res map {_._1})
		case 'Objects =>
			sender ! objects
			
	}
}