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
/**
 * Author: Brandon
 */
class BaseObject(startData:BaseObjectData, val map:WormholeMap, startSprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null){
	
	def this(x:Int, y:Int, prod:Int, defense:Int, map:WormholeMap, sprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) = 
		this(BaseObjectData(Location(x,y),prod,defense), map, sprite, clientConnection)
	def this(loc:Location, prod:Int, defense:Int, map:WormholeMap, sprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) = 
		this(BaseObjectData(loc,prod,defense), map, sprite, clientConnection)
		
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
	def setAllUnits(units:Map[PlayerId, Int]){
		ref ! units
	}
	def setSprite(sprite:(BaseObject) => Sprite){
		ref ! (sprite(this))
	}
	def attack(other:BaseObject){
		ref ! other
	}
	def sprite = fetch(spriteFuture)
	def spriteFuture = (ref ? 'Sprite).mapTo[Sprite]
	
	def units = fetch(unitsFuture)
	def unitsFuture = (ref ? 'Units).mapTo[Map[PlayerId, Int]]
	
	def setOwner(owner:PlayerId){
		ref ! ('Owner, owner)
	}
	
	def owner = fetch(ownerFuture)
	def ownerFuture = (ref ? 'Owner).mapTo[Option[PlayerId]]

	def addListener(listener:BaseObjectListener){ref ! listener}
	def removeListener(listener:BaseObjectListener){ref ! ('Remove, listener)}
}
 
/**
 * Actor backend for BaseObject.
 * Author: Brandon
 */
private class BaseObjectImpl(val data:BaseObjectData, var sprite:Sprite, val main:BaseObject) extends Actor{
	
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
					val owned = units.getOrElse(own, 0)
					val send = if(owned%2==0) owned/2 else owned/2+1
					if(send>0){
						units += ((own, owned-send))
						sendTo.unitArrival(send, own)
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
			listeners -= listener
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
		//total non-owner ships to determine whether owner wins
		val nonOwner = math.max(0, units.foldLeft(0) {(acc, tup) => if(tup._1 != ownerVal) acc + tup._2 else acc})
		val ownerCount = units.getOrElse(ownerVal,0)
		//if owner has more ships than all other players, leave only owner ships
		if(nonOwner==0||nonOwner<=ownerCount){
			val newOwnerCount = ownerCount-nonOwner
			units = Map() + ((ownerVal, newOwnerCount))
			listeners foreach {_.allUnitsChanged(main)}
		}else{
			//divide lost units among attackers
			var remainingLost = ownerCount
			//loop, as some players may not have enough units to lose the amount they should
			while(remainingLost>0){
				//determine total units lost for each non-owning player
				val nonOwnerPlayers = units count (_._1 != ownerVal)
				val baseRem = ownerCount/nonOwnerPlayers
				val perPlayer = if(ownerCount % nonOwnerPlayers == 0) baseRem else baseRem + 1
				//remove current owner from list of units - has been defeated
				units = units filterNot(_._1 == ownerVal)
				//remap units to new values
				units = units map {
					t =>
						val (p, c) = t
						remainingLost -= perPlayer
						val remainingAttacker = c-perPlayer
						if(remainingAttacker<0){
							/* If there were not enough units to satisfy the perPlayer amount,
							 * the 'remaining' amount, which is negative, is replaced. 
							 * abs(remainingAttacker) would be the amount of units this player
							 * was short
							 */
							remainingLost -= remainingAttacker
						}
						(p, math.max(0,c-perPlayer))
				}
				//remove any players without units
				units = units filter(_._2>0)
				listeners foreach {_.allUnitsChanged(main)}
				//new owner is player with the most units
				owner = Some(units.fold((-1, 0)) {
					(acc,t) =>
						val (p, c) = t
						if (c>acc._2) t else acc
				}._1)
				listeners foreach {_.ownerChanged(owner get, main)}
			}
		}
	}
	def unitArrival(amt:Int, owner:PlayerId){
		val current = units getOrElse(owner, 0)
		val newTot = current + amt
		if(this.owner.exists {_==owner}){
			units += ((owner, newTot))
			listeners foreach {_.unitsChanged(owner, amt, main)}
		}else{
			val add = math.max(0,amt-data.defense)
			units += ((owner, add + current))
			if(this.owner.isEmpty && add > 0){
				this.owner = Some(owner)
				listeners foreach {_.ownerChanged(owner, main)}
			}
			if(add>0){
				listeners foreach {_.unitsChanged(owner, add, main)}
			}
		}
	}
}

private class ClientBaseObjectImpl(data:BaseObjectData, sprite:Sprite, main:BaseObject, out:ThreadsafeMessageWriter) extends BaseObjectImpl(data,sprite, main){
	override def receive = ownReceive orElse super.receive
	
	def ownReceive:PartialFunction[Any, Unit] = {
		case 'Update =>
			
		case sendTo:BaseObject =>
			val otherDataFuture = sendTo.dataFuture
			val otherData = fetch(otherDataFuture)
			val msgType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.ATTACK).build()
			val attackMsg = GameProto.Attack.newBuilder()
			attackMsg setFromX data.location.x setFromY data.location.y setToX otherData.location.x setToY otherData.location.y
			out.write(msgType, attackMsg build())
	}
}