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
import wormhole.Player
import wormhole.PlayerId
import wormhole.IdProvider
import akka.actor.ActorLogging
/**
 * Stores data for a wormhole map.  This includes information on the objects in the map,
 * dimensions of the map, and the players for the map.
 */
class WormholeMap(val width:Int, val height:Int, val players:List[Player]){
	
	private[this] val ref:ActorRef = WormholeSystem.actorOf(Props(new WormholeMapImpl(this)))
	val idProvider:IdProvider = new IdProvider();
	/**
	 * Gets the player for the given player id.
	 */
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
	def isCompletelyFinished() =ref.isTerminated
	/**
	 * Gets the object at the given position.  Returns `None` if no object is at the given position,
	 * otherwise returns an instance of Some containing the object.
	 */
	def objectAt(x:Int, y:Int) = {
		fetch ((ref ? ('At, x,y)).mapTo[Option[BaseObject]])
	}
	
	def newGroupIdFuture() = idProvider.newIdFuture
	/**
	 * Generates a new group id.
	 */
	def newGroupId() = idProvider.newId
	def objectsFuture = (ref ? 'Objects).mapTo[List[BaseObject]]
	def objects = fetch (objectsFuture)
	def unitGroupsFuture = (ref ? 'Units).mapTo[List[UnitGroup]]
	def unitGroups = fetch (unitGroupsFuture)
	def removeGroup(groupId:Int){
		ref ! ('RemoveGroup, groupId)
	}
	def groupForIdFuture(groupId:Int) = (ref ? ('GroupForId, groupId)).mapTo[Option[UnitGroup]]
	/**
	 * Gets the unit group for the given id.
	 */
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
private class WormholeMapImpl(main:WormholeMap) extends Actor with ActorLogging{
	/**
	 * Number of UnitGroup updates per base update.  UnitGroup updates move
	 * groups across the map, while base updates produce units.
	 */
	val MovesPerBaseUpdate = 15
	var ugID = 0
	var objects = new ListBuffer[BaseObject]
	var unitGroups = new ListBuffer[UnitGroup]
	var listeners = new ListBuffer[WormholeMapListener]
	
	/**
	 * Count of the number of updates since the last time bases were updated.
	 */
	var count = 0
	def receive = {
		case obj:BaseObject =>
			objects += obj
		case ('AddListener, listen:WormholeMapListener) =>
			listeners += listen
		case ('RemoveListener, listen:WormholeMapListener) =>
			listeners -= listen
		case group:UnitGroup =>
		  	log.debug("New unit group, adding and informing listeners")
			unitGroups += group
			listeners foreach {_.newUnitGroup(main, group)}
		case ('RemoveGroup, group:Int) =>
			unitGroups = unitGroups filterNot {_.id == group}
		case ('GroupForId, id:Int) =>
			sender ! (unitGroups find {_.id==id})
		case 'Update =>
			log.debug("Updating map")
			//increase update count
			count += 1
			if(count==MovesPerBaseUpdate){
				log.debug("Updating bases on map")
				//if we are at an update for bases, do it
				objects foreach {_.update()}
				count = 0
			}
			unitGroups foreach {_.update}
			listeners foreach {_.updateComplete(main)}
			winner foreach {
			  win =>
			    log.debug("Winner found")
				listeners foreach {_.playerVictory(main, win)}
				context.stop(self)
			}
			log.info("Update complete")
		case ('At,x:Int,y:Int) =>
			log.debug("Looking for object at (" + x + "," + y + ")")
			//zip to their data
			val zipped = objects zip (objects map {_.dataFuture})
			//find an object with the give location
			val res = zipped find {
				tup =>
					val loc = fetch(tup._2).location
					loc.x==x && loc.y == y
			}
			log.info("Object: " + res)
			sender ! (res map {_._1})
		case 'Objects =>
			sender ! (objects toList)
		case 'Units =>
			sender ! (unitGroups toList)
	}
	def winner:Option[PlayerId] = {
		val controlledBases = objects.filterNot(_.owner == None)
		val check = controlledBases.head.owner
		if (controlledBases forall {_.owner == check}) check else None
	}
}
