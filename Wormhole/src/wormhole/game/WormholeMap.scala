package wormhole.game

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import wormhole.actor._
import wormhole.WormholeSystem
import com.google.protobuf.Message
import wormhole.game.network.GameProto
import scala.collection.mutable.ListBuffer
/**
 * Author: Brandon
 */
class WormholeMap(val width:Int, val height:Int, val players:List[Player]){
	
	val ref:ActorRef = WormholeSystem.actorOf(Props(new WormholeMapImpl(this)))
	
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
	
	def newGroupIdFuture() = (ref ? 'GroupId).mapTo[Int]
	def newGroupId() = fetch(newGroupIdFuture())
	def objectsFuture = (ref ? 'Objects).mapTo[List[BaseObject]]
	def objects = fetch (objectsFuture)
	def unitGroupsFuture = (ref ? 'Units).mapTo[List[UnitGroup]]
	def unitGroups = fetch (unitGroupsFuture)
	def removeGroup(groupId:Int){
		ref ! ('RemoveGroup, groupId)
	}
	def groupForIdFuture(groupId:Int) = (ref ? ('GroupForId, groupId)).mapTo[Option[UnitGroup]]
	def groupForId(groupId:Int) = fetch(groupForIdFuture(groupId))
	def addMapListener(listener:WormholeMapListener){
		ref ! ('AddListener, listener)
	}
	def removeMapListener(listener:WormholeMapListener){
		ref ! ('RemoveListener, listener)
	}
}
 
/**
 * Actor backend for WormholeMap.
 * Author: Brandon
 */
private class WormholeMapImpl(main:WormholeMap) extends Actor{
	val MovesPerBaseUpdate = 15
	var ugID = 0
	var objects = new ListBuffer[BaseObject]
	var unitGroups = new ListBuffer[UnitGroup]
	var listeners = new ListBuffer[WormholeMapListener]
	var count = 0
	def receive = {
		case obj:BaseObject =>
			objects += obj
		case ('AddListener, listen:WormholeMapListener) =>
			listeners += listen
		case ('RemoveListener, listen:WormholeMapListener) =>
			listeners -= listen
		case 'GroupId =>
			sender ! ugID
			ugID += 1
		case group:UnitGroup =>
			unitGroups += group
			listeners foreach {_.newUnitGroup(main, group)}
		case ('RemoveGroup, group:Int) =>
			unitGroups = unitGroups filterNot {_.id == group}
		case ('GroupForId, id:Int) =>
			sender ! (unitGroups find {_.id==id})
		case 'Update =>
			count += 1
			if(count==MovesPerBaseUpdate){
				objects foreach {_.update()}
				count = 0
			}
			unitGroups foreach {_.update}
			listeners foreach {_.updateComplete(main)}
		case ('At,x:Int,y:Int) =>
			val zipped = objects zip (objects map {_.dataFuture})
			val res = zipped find {
				tup =>
					val loc = fetch(tup._2).location
					loc.x==x && loc.y == y
			}
			sender ! (res map {_._1})
		case 'Objects =>
			sender ! (objects toList)
		case 'Units =>
			sender ! (unitGroups toList)
	}
}
