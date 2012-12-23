package wormhole.game.network

import java.net.Socket
import wormhole.game.WormholeMap
import GameProto.MessageType._
import scala.collection.JavaConversions._
import java.io.IOException
import wormhole.Player
import java.awt.Color
import wormhole.ThreadsafeMessageWriter
import wormhole.SocketInfoData
import wormhole.game.UnitGroup
import wormhole.game.BaseObject
import wormhole.game.Location
import wormhole.lobby.WormholeMainClient
import javax.swing.JOptionPane
import com.wormhole.network.PlayerProto
import org.slf4j.LoggerFactory

/**
 * Client-side connection for in game.  Handles forwarding messages to bases and unit groups.
 */
class ClientPlayerConnection(val socketData:SocketInfoData, mapReadyCallback:() => Unit) extends Runnable{
  
	def in = socketData.in
	def out = socketData.out
	
	val log = LoggerFactory.getLogger("ClientPlayerConnection")
	
	private var gameMap:WormholeMap = null
	
	def map = gameMap
	
	/**
	 * The Player representing this client.
	 */
	private var pData:Option[Player] = None
	
	def playerData = pData
	
	private var running = false
	
	def isRunning = running
	
	def run(){
		log.debug("Starting connection")
		running = true
		
		//read map and initialize
		val gameMapProto = GameProto.Map.parseDelimitedFrom(in)
		gameMap = protocolToMap(gameMapProto, out)
		
		log.debug("Map read and set")
		
		//tell callback map is ready
		mapReadyCallback()
		
		log.info("Map ready callback complete")
		
		//get player, then do the game loop
		val player = PlayerProto.Player.parseDelimitedFrom(in)
		this.pData = Some(new Player(player.getName(), player.getId(), new Color(player.getColor())))
		
		log.debug("Read player data")
		
		basicLoop()
		
		log.debug("Connection loop complete")
		
		running = false
	}
	private var continue = true
	def basicLoop(){
		log.info("Beginning connection loop")
		while(continue){
		try{
			val msg = GameProto.IncomingMessageType.parseDelimitedFrom(in).getType()
			msg match{
				case UNIT_ARRIVAL =>
					log.debug("Received UNIT_ARRIVAL message")
				  	val changeMsg = GameProto.UnitArrival.parseDelimitedFrom(in)
				  	log.info("Read UNIT_ARRIVAL message data")
					val changeData = changeMsg.getArrived() //The unit info
					val baseObj = map.objectAt(changeMsg getX, changeMsg getY)
					val player = map.players find {_.id == changeData.getOwner()}					
					player foreach {
						owner =>
							baseObj foreach {_.unitArrival(changeData.getCount(), owner.id)}
					}
				case OWNER_CHANGE =>
				  	log.debug("Received OWNER_CHANGE message")
					val changeMsg = GameProto.OwnerChange.parseDelimitedFrom(in)
					log.info("Read OWNER_CHANGE message data")
					val newOwner = map.players find {_.id == changeMsg.getNewOwnerId()}
					val baseObj = map.objectAt(changeMsg getX, changeMsg getY)
						newOwner foreach {
						owner =>
							baseObj foreach {_.setOwner(owner.id)}
					}
				case ALL_UNIT_CHANGE =>
				  	log.debug("Received ALL_UNIT_CHANGE message")
					val changeMsg = GameProto.AllUnitChange.parseDelimitedFrom(in)
					log.info("Read ALL_UNIT_CHANGE message data")
					val baseObj = map.objectAt(changeMsg.getX(), changeMsg.getY())
					
					//converts the protocol message to a map of player ID's to unit counts
					//which can be used to set the units for the base
					val unitMap = unitInfoListProtoToBaseList(changeMsg.getUnitInfoList(), map.players)
					baseObj foreach {
						obj =>
							obj.setAllUnits(unitMap)
					}
				case PLAYER_DATA =>
				  	log.debug("Received PLAYER_DATA message")
					val playerData = PlayerProto.Player.parseDelimitedFrom(in)
					log.info("Read PLAYER_DATA message daata")
					this.pData = Some(new Player(playerData.getName(), playerData.getId(), new Color(playerData.getColor())))
				case NEW_UNIT_GROUP =>
				  	log.debug("Received NEW_UNIT_GROUP message")
					val ugData = GameProto.NewUnitGroup.parseDelimitedFrom(in)
				  	log.info("Read NEW_UNIT_GROUP message data")
					val group = new UnitGroup(ugData.getId(), ugData.getOwner(), ugData.getCount(), BaseObject.genBasicUnitSprite(map), map, null, null)
					group.setLocation(Location(ugData.getX(), ugData.getY()))
					map.addUnitGroup(group)
				case UNIT_GROUP_POSITION =>
				  	log.debug("Received UNIT_GROUP_POSITION message")
					val ugp = GameProto.UnitGroupPosition.parseDelimitedFrom(in)
					log.info("Read UNIT_GROUP_POSITION message data")
					if(ugp.getComplete()){
						//if the group has finished moving remove it
						map.removeGroup(ugp.getId())
						log.debug("Unit group complete: removed group")
					}else{
						log.debug("Finding unit group to update")
						//otherwise update
						map.groupForId(ugp.getId()) foreach {
							ug =>
								val loc = Location(ugp.getX(), ugp.getY())
								ug.setLocation(loc)
								log.debug("Updated unit group")
						}						
					}
				case DISCONNECT =>
				  	log.info("Received DISCONNECT message")
					continue = false
				case LEAVE_GAME =>
				  	log.info("Received LEAVE_GAME message")
					continue = false
					val conf = GameProto.IncomingMessageType.newBuilder().setType(CONFIRM_LEAVE_GAME).build()
					out.write(conf)
					log.debug("Sent confirmation of leave game message: starting main client instance")
					new Thread(new WormholeMainClient(socketData)).start()
				case CONFIRM_LEAVE_GAME =>
				  	log.info("Received CONFIRM_LEAVE_GAME: starting main client instance")
					continue = false
					new Thread(new WormholeMainClient(socketData), "CPC MainClient").start()
				case PLAYER_VICTORY =>
				  	log.debug("Received PLAYER_VICTORY message")
					val winner = GameProto.Victory.parseDelimitedFrom(in).getWinnerId()
					log.info("Read PLAYER_VICTORY message data")
					//TODO improve victory/defeat display
					JOptionPane.showMessageDialog(null, if(playerData exists {_.id==winner}) "Victory" else "Defeat")
				case _ =>
			}
			}catch{
				case e:IOException =>
					log.warn("IOException in client connection to game.", e)
					continue = false
			}
		}
	}
	def leaveGame(){
		log.debug("Sending LEAVE_GAME message")
		val msg = GameProto.IncomingMessageType.newBuilder().setType(LEAVE_GAME).build()
		out.write(msg)
	}
	def disconnect(){
		log.debug("Sending DISCONNECT message and stopping connection")
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.DISCONNECT).build()
		continue = false
		out.write(msgType)
	}
}
