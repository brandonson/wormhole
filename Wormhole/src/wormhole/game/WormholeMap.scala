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
	def addUnitGroup(group:UnitGroup){
		ref ! group
	}
	def updateAll(){
		ref ! 'Update
	}
	def objectAt(x:Int, y:Int) = {
		fetch ((ref ? ('At, x,y)).mapTo[Option[BaseObject]])
	}
	def objectsFuture = (ref ? 'Objects).mapTo[List[BaseObject]]
	def objects = fetch (objectsFuture)
	def unitGroupsFuture = (ref ? 'Units).mapTo[List[UnitGroup]]
	def unitGroups = fetch (unitGroupsFuture)
}
 
/**
 * Actor backend for WormholeMap.
 * Author: Brandon
 */
private class WormholeMapImpl extends Actor{
	val MovesPerBaseUpdate = 5
	
	var objects:List[BaseObject] = Nil
	var unitGroups:List[UnitGroup] = Nil
	var count = 0
	def receive = {
		case (obj:BaseObject) =>
			objects ::= obj
		case (group:UnitGroup) =>
			unitGroups ::= group
		case 'Update =>
			count += 1
			if(count==MovesPerBaseUpdate){
				objects foreach {_.update()}
				count = 0
			}
			unitGroups foreach {_.update()}
			unitGroups = unitGroups filterNot {_.isComplete}
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
		case 'Units =>
			sender ! unitGroups
	}
}