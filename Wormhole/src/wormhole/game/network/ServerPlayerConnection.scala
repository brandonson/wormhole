package wormhole.game.network

import wormhole.game.Player
import java.net.Socket
import GameProto.MessageType._
import wormhole.game.BaseObjectListener
import wormhole.game.BaseObject
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import wormhole.game.WormholeMap
import wormhole.ThreadsafeMessageWriter
import wormhole.actor._
import wormhole.game.PlayerId
import wormhole.SocketInfoData
import akka.actor.Actor
import wormhole.WormholeSystem
import akka.actor.Props
import java.io.IOException

class ServerPlayerConnection(val player:Player, val map:WormholeMap, val socketData:SocketInfoData) extends Runnable with BaseObjectListener{

	private val ref = WormholeSystem.actorOf(Props(new ListenerImpl(this)))
	def in = socketData.in
	def out = socketData.out
	def run(){
		map.objects foreach {obj => obj.addListener(this)}
		val inProto = mapToProtocol(map)
		out.write(inProto)
		val pdata = GameProto.Player.newBuilder()
		pdata.setId(player.id).setColor(player.color.getRGB)
		out.write(pdata.build())
		try{
			basicLoop()
		}catch{
			case _:IOException =>
		}
		map.objects foreach {_.removeListener(this)}
		out.close()
	}
	
	private def basicLoop(){
		var continue = true
		while(continue){
			GameProto.IncomingMessageType.parseDelimitedFrom(in).getType() match{
				case ATTACK =>
					val attackMsg = GameProto.Attack.parseDelimitedFrom(in)
					val obj = map.objectAt(attackMsg.getFromX(), attackMsg.getFromY())
					val isOwned = obj exists {_.owner exists {_ == player.id}}
					if(isOwned){
						val attackTo = map.objectAt(attackMsg.getToX(), attackMsg.getToY())
						attackTo foreach {
							attack =>
								obj.get.attack(attack)
						}
					}
				case DISCONNECT =>
					continue = false
				case SEND_PLAYER_DATA =>
					val data = GameProto.Player.newBuilder()
					data.setId(player.id)
					data.setColor(player.color.getRGB())
					val built = data.build()
					val msgType = GameProto.IncomingMessageType.newBuilder().setType(PLAYER_DATA).build()
					out.write(msgType, built)
				case _ =>
			}
		}
	}
	def ownerChanged(newOwner:PlayerId, changeObj:BaseObject){
		ref ! ('OwnerChanged, newOwner, changeObj)
	}
	def unitsChanged(player:PlayerId, amt:Int, obj:BaseObject){
		ref ! ('UnitsChanged, player, amt, obj)
	}
	def allUnitsChanged(obj:BaseObject){
		ref ! ('AllUnits, obj)
	}
}

private class ListenerImpl(conn:ServerPlayerConnection) extends Actor{
	
	def receive = {
		case ('OwnerChanged,newOwner:PlayerId, changeObj:BaseObject) =>
			ownerChanged(newOwner, changeObj)
		case ('UnitsChanged, player:PlayerId, amt:Int, obj:BaseObject) =>
			unitsChanged(player, amt, obj)
		case ('AllUnits, obj:BaseObject) =>
			allUnitsChanged(obj)
	}
	
	def ownerChanged(newOwner:PlayerId, changeObj:BaseObject){
		val changeBuilder = GameProto.OwnerChange.newBuilder()
		val changedOn = changeObj.data.location
		changeBuilder.setX(changedOn.x)
		changeBuilder.setY(changedOn.y)
		changeBuilder.setNewOwnerId(newOwner)
		val data = changeBuilder.build()
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(OWNER_CHANGE).build()
		conn.out.write(msgType, data)
	}
	def unitsChanged(player:PlayerId, amt:Int, obj:BaseObject){
		val changeBuilder = GameProto.UnitArrival.newBuilder()
		val data = obj.data
		changeBuilder.setX(data.location.x)
		changeBuilder.setY(data.location.y)
		
		val infBuilder = GameProto.UnitInfo.newBuilder()
		infBuilder.setOwner(player)
		infBuilder.setCount(amt)
		changeBuilder.setArrived(infBuilder.build())
		
		val change = changeBuilder.build()
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(UNIT_ARRIVAL).build()
		conn.out.write(msgType, change)
	}
	
	def allUnitsChanged(obj:BaseObject){
		val changeBuilder = GameProto.AllUnitChange.newBuilder()
		val data = obj.data
		val ownerFuture = obj.ownerFuture
		lazy val owner = fetch(ownerFuture)
		changeBuilder setX data.location.x
		changeBuilder setY data.location.y
		val lb = new ListBuffer[GameProto.UnitInfo]
		lb ++= obj.units.map{
			tup =>
				val (owner, count) = tup
				val infBuilder = GameProto.UnitInfo.newBuilder()
				infBuilder setOwner owner
				infBuilder setCount count
				infBuilder.build()
		}
		changeBuilder.addAllUnitInfo(lb)
		val change = changeBuilder.build()
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(ALL_UNIT_CHANGE).build()
		conn.out.write(msgType, change)
	}
}