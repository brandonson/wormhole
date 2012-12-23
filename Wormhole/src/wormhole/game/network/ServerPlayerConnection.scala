package wormhole.game.network

import wormhole.Player
import wormhole.PlayerId
import java.net.Socket
import GameProto.MessageType._
import wormhole.game.BaseObjectListener
import wormhole.game.BaseObject
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import wormhole.game.WormholeMap
import wormhole.ThreadsafeMessageWriter
import wormhole.actor._
import wormhole.SocketInfoData
import akka.actor.Actor
import wormhole.WormholeSystem
import akka.actor.Props
import java.io.IOException
import wormhole.game.WormholeMapListener
import wormhole.game.UnitGroup
import wormhole.lobby.WormholeClientHandler
import wormhole.WormholeServer
import com.wormhole.network.PlayerProto
import org.slf4j.LoggerFactory
import akka.actor.ActorLogging

/**
 * Connection handler on the server for individual clients in game.  Handles updating the client with changes to bases and
 * unit groups.
 */
class ServerPlayerConnection(val player:Player, val map:WormholeMap, val socketData:SocketInfoData) 
	extends Runnable with BaseObjectListener with WormholeMapListener{

	val log = LoggerFactory.getLogger("ServerPlayerConnection")
  
	/**
	 * Backend actor.
	 */
	private val ref = WormholeSystem.actorOf(Props(new ListenerImpl(this)))
	
	def in = socketData.in
	def out = socketData.out
	def run(){
		log.info("Starting connection")
		//register as a listener to receive updates
		map.addMapListener(this)
		map.objects foreach {_.addListener(this)}
		
		log.debug("Entering loop")
		
		//do server loop to read and handle messages
		try{
			basicLoop()
		}catch{
			case e:IOException =>
			  log.warn("Exception while running connection.", e)
		}
		/* unregister as a listener.
		 * only if not due to game end - 
		 * map actor stops on game completion
		 * and this would block
		 */
		if(!map.isCompletelyFinished){
			log.debug("Removing self as listener to map")
			map.removeMapListener(this)
			map.objects foreach {_.removeListener(this)}
		}
		log.info("Loop completed")
	}
	
	private def basicLoop(){
		log.debug("Beginning loop")
		var continue = true
		while(continue){
			val mType = GameProto.IncomingMessageType.parseDelimitedFrom(in).getType() 
			mType match{
				case ATTACK =>
				  	log.debug("Received ATTACK message")
					val attackMsg = GameProto.Attack.parseDelimitedFrom(in)
					log.info("Read ATTACK message data")
					val obj = map.objectAt(attackMsg.getFromX(), attackMsg.getFromY())
					val isOwned = obj exists {_.owner exists {_ == player.id}}
					if(isOwned){
						log.debug("Ownership of attack source confirmed")
						//if the client owns the base, order an attack
						val attackTo = map.objectAt(attackMsg.getToX(), attackMsg.getToY())
						attackTo foreach {
							attack =>
								obj.get.attack(attack)
								log.debug("Launching attack")
						}
					}
				case DISCONNECT =>
				  	log.info("Received DISCONNECT message")
					continue = false
					out.close()
					socketData.socket.close()
					log.debug("Disconnect complete")
				case SEND_PLAYER_DATA =>
				  	log.info("Received SEND_PLAYER_DATA message")
					val data = PlayerProto.Player.newBuilder()
					data.setId(player.id)
					data.setColor(player.color.getRGB())
					val built = data.build()
					val msgType = GameProto.IncomingMessageType.newBuilder().setType(PLAYER_DATA).build()
					out.write(msgType, built)
					log.debug("Player data sent")
				case LEAVE_GAME =>
				  	log.info("Received LEAVE_GAME message")
					ref ! 'Leave
					continue = false
				case CONFIRM_LEAVE_GAME =>
				  	log.info("Received CONFIRM_LEAVE_GAME message")
					ref ! 'LeaveConfirm
					continue = false
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
	def newUnitGroup(map:WormholeMap, group:UnitGroup){
		ref ! ('NewGroup, group)
	}
	def updateComplete(m:WormholeMap){
		ref ! 'Update
	}
	
	def playerVictory(map:WormholeMap, winner:PlayerId){
		ref ! ('Victory, winner)
	}
	
	def leaveGame(){
		ref ! 'TellLeave
	}
}

private class ListenerImpl(val conn:ServerPlayerConnection) extends Actor with ActorLogging{
	
	self ! 'DoInit //send map info
	
	private[this] val groups = new ListBuffer[UnitGroup]
	private[this] var active = true
	def receive = {
		case 'DoInit =>
		  	log.debug("Initializing")
			sendInitInfo()
		case ('OwnerChanged,newOwner:PlayerId, changeObj:BaseObject) =>
			if(active) {
				log.info("Changing object owner to " + newOwner)
				ownerChanged(newOwner, changeObj)
			}
		case ('UnitsChanged, player:PlayerId, amt:Int, obj:BaseObject) =>
			if(active){
				log.info("Changing units for an object to " + amt + " for " + player)
				unitsChanged(player, amt, obj)
			}
		case ('AllUnits, obj:BaseObject) =>
			if(active){
				log.info("Changing all units for an object")
				allUnitsChanged(obj)
			}
		case ('NewGroup, group:UnitGroup) =>
			if(active){
				log.info("Creating new unit group")
				groups += group
				val mType = GameProto.IncomingMessageType.newBuilder().setType(NEW_UNIT_GROUP)
				val msg = GameProto.NewUnitGroup.newBuilder()
				msg.setCount(group.count).setId(group.id).setOwner(group.owner)
				val loc = group.location
				msg.setX(loc.x)
				msg.setY(loc.y)
				conn.out.write(mType.build(), msg.build())
				log.debug("Group created and message sent")
			}
		case 'Update =>
			if(active){
				log.info("Map has been updated.  Informing client of unit group changes.")
				val typeMsg = GameProto.IncomingMessageType.newBuilder().setType(UNIT_GROUP_POSITION).build()
				groups foreach {
					grp =>
						val msg = GameProto.UnitGroupPosition.newBuilder()
						msg.setId(grp.id)
						if(grp.isComplete){
							msg.setComplete(true)
						}else{
							val loc = grp.location
							msg.setX(loc.x).setY(loc.y)
						}
						conn.out.write(typeMsg, msg.build())
				}
				log.debug("Unit group positions sent")
				groups filterNot {_.isComplete}
				log.debug("Completed groups removed from list")
			}
		case ('Victory, winner:PlayerId) =>
			if(active){
				log.info("Victory for player " + winner)
				val msgType = GameProto.IncomingMessageType.newBuilder().setType(PLAYER_VICTORY).build()
				val victoryBuilder = GameProto.Victory.newBuilder()
				victoryBuilder.setWinnerId(winner)
				conn.out.write(msgType, victoryBuilder.build())
				log.debug("Sent victory message to client")
			}
		case 'Leave =>
			if(active){
				log.info("Client requested leave.  Will confirm")
				active = false
				conn.map.removeMapListener(conn)
				val conf = GameProto.IncomingMessageType.newBuilder().setType(CONFIRM_LEAVE_GAME).build()
				conn.out.write(conf)
				WormholeServer.mainServer.handleNewConnection(conn.socketData)
				log.debug("Leave game confirmed, main server told to set up communication with client")
			}
		case 'LeaveConfirmed =>
		  	log.info("Leave confirmed by client.  Transferring over to main server")
			WormholeServer.mainServer.handleNewConnection(conn.socketData)
		case 'TellLeave =>
			if(active){
				log.info("Server is telling client to leave game.")
				active = false
				conn.map.removeMapListener(conn)
				val msg = GameProto.IncomingMessageType.newBuilder().setType(LEAVE_GAME).build()
				conn.out.write(msg)
				log.debug("LEAVE_GAME message sent")
			}
	}
	
	def sendInitInfo(){
		log.debug("Sending initialization info")
		//send map to client
		val inProto = mapToProtocol(conn.map)
		conn.out.write(inProto)
		
		log.debug("Sent initial map")
		
		//tell client which player they are
		val pdata = PlayerProto.Player.newBuilder()
		pdata.setId(conn.player.id).setName(conn.player.name).setColor(conn.player.color.getRGB)
		conn.out.write(pdata.build())
		
		log.debug("Sent client player info")
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
		log.debug("Sent OWNER_CHANGE message for an object with new owner " + newOwner)
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
		
		log.debug("Sent UNIT_ARRIVAL message for an object with " + amt + " arriving units for player " + player)
	}
	
	def allUnitsChanged(obj:BaseObject){
		val changeBuilder = GameProto.AllUnitChange.newBuilder()
		val ownerFuture = obj.ownerFuture
		lazy val owner = fetch(ownerFuture)
		val data = obj.data
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
		
		log.debug("Sent ALL_UNIT_CHANGE message for object at " + data.location)
	}
}