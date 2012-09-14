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

object BaseObject{
	def genBasicUnitSprite(map:WormholeMap)(group:UnitGroup) = new Sprite{
		def render(g:Graphics2D, x:Int, y:Int){
			g.setColor(map.player(group.owner) map {_.color} getOrElse Color.GRAY)
			g.fillRect(x-5, y-5, 10, 10)
		}
	}
}

/**
 * Author: Brandon
 */
class BaseObject(startData:BaseObjectData, val map:WormholeMap, startSprite:(BaseObject) => Sprite, clientConnection:ThreadsafeMessageWriter = null) extends Updateable{
	
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
	val PercentageDiv = 50
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
					val owned = units.getOrElse(own, 0)
					val send = if(owned%2==0) owned/2 else owned/2+1
					if(send>0){
						units += ((own, owned-send))
						val group = new UnitGroup(main.map.newGroupId(), own,send, BaseObject.genBasicUnitSprite(main.map) _, main.map, data.location, sendTo.data.location)
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
		val ownerCount = units.getOrElse(ownerVal,0)
		val withoutOwner = units filterNot {_._1==ownerVal}
		//maps to list of tuples ((nonOwner, remainingForNonOwner), killedOfOwner)
		val results = withoutOwner map {
			tup =>
				val (otherOwner, otherStart) = tup
				val otherCount = otherStart-this.data.defense
				if(otherCount<=0){
					((otherOwner, 0), ownerCount)
				}else if(ownerCount<=0){
					((otherOwner, otherCount), 0)
				}else{
					val ownerHasMore = ownerCount>otherCount
					val otherPOfOwner = (otherCount*100)/(ownerCount)
					val ownerPOfOther = (ownerCount*100)/(otherCount)
					val percentOfOwnerKilled = otherPOfOwner*100/PercentageDiv
					val killed = math.max(MinKill, percentOfOwnerKilled*ownerCount/1000)
					val percentOfOtherKilled = ownerPOfOther*100/PercentageDiv
					val otherKilled = math.max(MinKill, percentOfOtherKilled*otherCount/1000)
					((otherOwner, otherCount-otherKilled), killed)
				}
		}
		val otherResults = results map {_._1}
		val ownerLoss = results.foldLeft(0){(acc, res) => acc + res._2}
		val ownerRem = math.max(0,ownerCount-ownerLoss)
		units = Map()
		if(ownerRem>0){
			units += ((ownerVal, ownerRem))
		}
		units ++= otherResults filterNot {_._2<=0}
		listeners foreach {_.allUnitsChanged(this.main)}
		if(units.size==1){
			owner = Some(units.head._1)
			listeners foreach {_.ownerChanged(owner.get, this.main)}
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