package wormhole.game

import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import akka.pattern.ask
import wormhole.actor._
import wormhole.graphics.Sprite
import wormhole.game.network.GameProto
import wormhole.ThreadsafeMessageWriter
import wormhole.WormholeSystem
import java.awt.Graphics2D
import java.awt.Color
import wormhole.Player
import wormhole.PlayerId
/**
 * Defines basic generation methods and constants for all BaseObjects.  As customizability and different types of objects
 * increase, these may disappear.
 */
object BaseObject{
	/**
	 * Generates a Sprite for representing a unit group
	 */
	def genBasicUnitSprite(map:WormholeMap)(group:UnitGroup) = new Sprite{
		def render(g:Graphics2D, x:Int, y:Int){
			//TODO more appealing sprite
			g.setColor(map.player(group.owner) map {_.color} getOrElse Color.GRAY)
			g.fillRect(x-5, y-5, 10, 10)
		}
	}
}

/**
 * Stores data on bases on a WormholeMap.  This includes information on location, owner, and sprite of the BaseObject.  The `startSprite`
 * parameter takes a generator for a sprite, as most sprites will depend on information from the BaseObject which is not available
 * before the constructor is invoked.
 */
/*
 * FIXME the clientConnection parameter should not exist
 * However, to remove it would require some other method
 * of determining whether to use a client implementation
 * or a server implementation.  Possible solutions include
 * a boolean parameter or a subclass of BaseObject that
 * overrides ref.
 */
class BaseObject(startData:BaseObjectData, val map:WormholeMap, startSprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) extends Updateable{
	
	def this(x:Int, y:Int, prod:Int, defense:Int, map:WormholeMap, sprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) = 
		this(BaseObjectData(Location(x,y),prod,defense), map, sprite, clientConnection)
	def this(loc:Location, prod:Int, defense:Int, map:WormholeMap, sprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) = 
		this(BaseObjectData(loc,prod,defense), map, sprite, clientConnection)
		
	/**
	 * The backend actor.  Handles storage and updating of data.  Updates to data
	 * includes handling battle resolution and sending units to other bases.
	 */
 	val ref:ActorRef = WormholeSystem.actorOf(Props(
 			if(clientConnection != null) 
 				new ClientBaseObjectImpl(startData, startSprite(this), this, clientConnection) 
 			else 
 				new BaseObjectImpl(startData,startSprite(this), this)))
 	/**
 	 * Retrieves the basic data for this object
 	 */
 	def data = fetch(dataFuture)
 	/**
 	 * Gets a future for the basic data for this object
 	 */
 	def dataFuture = (ref ? 'Data).mapTo[BaseObjectData]
 	/**
 	 * Updates, resolving battles and creating units
 	 */
 	def update() = ref ! 'Update
 	/**
 	 * Tells this object that units have arrived.  This will result
 	 * in a battle at the next update if the player is not the same as
 	 * the owner.
 	 */
 	def unitArrival(count:Int, player:PlayerId){
		ref ! (count, player)
	}
	/**
	 * Sets all the units on this BaseObject.  
	 */
	def setAllUnits(units:Map[PlayerId, Int]){
		ref ! units
	}
	/**
	 * Sets the sprite for this BaseObject.  Note that this
	 * uses a generator for easier compatibility with
	 * the constructor
	 */
	def setSprite(sprite:(BaseObject) => Sprite){
		ref ! (sprite(this))
	}
	/**
	 * Sends units to another base.
	 */
	def attack(other:BaseObject){
		ref ! other
	}
	/**
	 * Gets the sprite for this base
	 */
	def sprite = fetch(spriteFuture)
	def spriteFuture = (ref ? 'Sprite).mapTo[Sprite]
	
	/**
	 * Gets the map of units at this base.
	 */
	def units = fetch(unitsFuture)
	def unitsFuture = (ref ? 'Units).mapTo[Map[PlayerId, Int]]
	
	/**
	 * Sets the owner to the given player.
	 */
	def setOwner(owner:PlayerId){
		ref ! ('Owner, owner)
	}
	
	/**
	 * Gets the owner of this base.
	 */
	def owner = fetch(ownerFuture)
	def ownerFuture = (ref ? 'Owner).mapTo[Option[PlayerId]]

	def addListener(listener:BaseObjectListener){ref ! listener}
	def removeListener(listener:BaseObjectListener){ref ! ('Remove, listener)}
}
 
/**
 * Actor backend for BaseObject.  This includes an algorithm for determining how many units are destroyed in
 * a battle.  This algorithm works by first removing units destroyed by the defense value of the base.
 * Then, if there are still attacking units, each attacking player's units fight
 * the owner individually.  The number of units killed is the greatest of either `MinKill`, or the number
 * calculated by the following method:  The percentage of the single enemy's units that the player has
 * is divided by `PercentageDiv`, and that number is used as the percentage of units enemy units to remove.
 * This causes players with massively larger groups of units to lose only MinKill, as the enemy has only a
 * small percentage of their units, while the larger group would likely have enough to destroy all the opposing units in
 * one battle.
 * Author: Brandon
 */
private class BaseObjectImpl(val data:BaseObjectData, var sprite:Sprite, val main:BaseObject) extends Actor{
	/**
	 * Constant for battle algorithm.  Increasing will result in smaller percentages of ships being destroyed, 
	 * therefore causing slower battles.
	 */
	val PercentageDiv = 50
	/**
	 * Minimum number of ships destroyed by a player
	 */
	val MinKill = 10
	var units:Map[PlayerId, Int] = Map()
	var owner:Option[PlayerId] = None
	var listeners:List[BaseObjectListener] = Nil
	def receive = {
		case 'Data =>
			sender ! data
		case 'Update =>
			update()
		case sendTo:BaseObject =>
			owner foreach {
				own =>
					//calculate half the number of the owners units
					val owned = units.getOrElse(own, 0)
					val send = if(owned%2==0) owned/2 else owned/2+1
					if(send>0){
						//create a unit group for the units
						units += ((own, owned-send))
						val group = new UnitGroup(main.map.newGroupId(), own,send, BaseObject.genBasicUnitSprite(main.map) _, main.map, data.location, sendTo.data.location)
						//add to map and notify listeners that the number of units at this base
						//has changed
						main.map.addUnitGroup(group)
						listeners foreach {_.unitsChanged(own, -send, this.main)}
					}
			}
		case (unitCount:Int, owner:PlayerId) =>
			this.unitArrival(unitCount, owner)
		case units:Map[PlayerId, Int] =>
			this.units = units
		case newSprite:Sprite =>
			sprite = newSprite
		case 'Sprite =>
			sender ! sprite
		case 'Units =>
			sender ! units
		case ('Owner,owner:PlayerId) =>
			this.owner = Some(owner)
		case 'Owner =>
			sender ! owner
		case listener:BaseObjectListener =>
			listeners ::= listener
		case ('Remove, listener:BaseObjectListener) =>
			listeners filterNot {_==listener}
	}
	
	def update(){
		owner foreach {
			o =>
				//create units
				unitArrival(data.productivity, o)
				
				//check if there are multiple unit owners at this object
				//and if so, resolve battle
				if(units.count(_._2>0)>1){
					resolveMultiplePlayers()
				}
		}
	}

	
	protected def resolveMultiplePlayers(){
		val ownerVal = this.owner.get
		
		//number of units the owner has
		val ownerCount = units.getOrElse(ownerVal,0)
		
		//map of units without the owner included
		val withoutOwner = units filterNot {_._1==ownerVal}
		
		//maps to list of tuples ((nonOwner, remainingForNonOwner), killedOfOwner)
		val results = withoutOwner map {
			tup =>
				val (otherOwner, otherStart) = tup
				
				//units remaining after defenses are used
				val otherCount = otherStart-this.data.defense
				//handle cases where owner or attacker have no units.
				if(otherCount<=0){
					((otherOwner, 0), ownerCount)
				}else if(ownerCount<=0){
					((otherOwner, otherStart), 0) //don't kill units if there were no owner units to fight
				}else{
					//percentage of ships owned by one player in relation to the other
					val otherPOfOwner = (otherCount*100)/(ownerCount)
					val ownerPOfOther = (ownerCount*100)/(otherCount)
					
					//percentage of ships is divided by constant, then the number of ships is calculated
					//percentage killed is multiplied by ship count, then divided by 1000 to remove
					//the multiplications by 100 that were used to keep numbers as integers
					val percentOfOwnerKilled = otherPOfOwner*100/PercentageDiv
					val ownerKilled = math.max(MinKill, percentOfOwnerKilled*ownerCount/1000)
					val percentOfOtherKilled = ownerPOfOther*100/PercentageDiv
					val otherKilled = math.max(MinKill, percentOfOtherKilled*otherCount/1000)
					((otherOwner, otherCount-otherKilled), ownerKilled)
				}
		}
		//map to list of (owner, units) for all players except owner
		val otherResults = results map {_._1}
		
		//calculate total losses for owner
		val ownerLoss = results.foldLeft(0){(acc, res) => acc + res._2}
		
		//remaining units for owner
		val ownerRem = math.max(0,ownerCount-ownerLoss)
		
		//rebuild unit map
		units = Map()
		units ++= otherResults filterNot {_._2<=0}
		if(ownerRem>0){
			//there are units for the current owner, keep them
			units += ((ownerVal, ownerRem))
		}else{
			//determine new owner (player with most units)
			val newOwner = units.foldLeft[Option[(PlayerId, Int)]](None){
				(acc, check) =>
					Some( acc map {
						accVal =>
							val (accId, accCount) = accVal
							val (id, count) = check
							if(count > accCount) check else accVal
					} getOrElse (check))
			}.map {_._1}
			//if there's no players with units left, there will be no newOwner determined
			//so keep the old one
			owner = Some(newOwner getOrElse owner.get)
		}
		listeners foreach {_.allUnitsChanged(this.main)}
	}
	def unitArrival(amt:Int, owner:PlayerId){
		val current = units getOrElse(owner, 0)
		val newTot = current + amt
		if(this.owner.exists {_==owner}){
			//if they're units for the current owner, add them
			units += ((owner, newTot))
			listeners foreach {_.unitsChanged(owner, amt, main)}
		}else{
			//otherwise, remove units based on planet defenses
			//then check whether we even have an owner.
			//if there is no owner, and there are units left,
			//the player who owns the units becomes owner
			val add = math.max(0,amt-data.defense)
			units += ((owner, add + current))
			//owner check
			if(this.owner.isEmpty && add > 0){
				this.owner = Some(owner)
				listeners foreach {_.ownerChanged(owner, main)}
			}
			//if we have units left, tell listeners that there are new units
			if(add>0){
				listeners foreach {_.unitsChanged(owner, add, main)}
			}
		}
	}
}

/**
 * Backend implementation for client-side actors.  Overrides attacks to send messages to server to initiate them.  Messages
 * are sent to `out`.
 */
private class ClientBaseObjectImpl(data:BaseObjectData, sprite:Sprite, main:BaseObject, val out:ThreadsafeMessageWriter) extends BaseObjectImpl(data,sprite, main){
	override def receive = ownReceive orElse super.receive
	
	def ownReceive:PartialFunction[Any, Unit] = {
		case 'Update =>
			
		case sendTo:BaseObject =>
			//Launch an attack.
			
			//Get information on the other base
			val otherData = sendTo.data
			
			//create message, then send
			val msgType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.ATTACK).build()
			val attackMsg = GameProto.Attack.newBuilder()
			attackMsg setFromX data.location.x setFromY data.location.y setToX otherData.location.x setToY otherData.location.y
			out.write(msgType, attackMsg build())
	}
}
