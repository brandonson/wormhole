package wormhole.game.network

import java.net.Socket
import wormhole.game.WormholeMap
import GameProto.MessageType._
import scala.collection.JavaConversions._
import java.io.IOException
import wormhole.game.Player
import java.awt.Color
import wormhole.ThreadsafeMessageWriter
import wormhole.SocketInfoData
import wormhole.game.UnitGroup
import wormhole.game.BaseObject
import wormhole.game.Location
import wormhole.lobby.WormholeMainClient
import javax.swing.JOptionPane
import com.wormhole.network.PlayerProto

/**
 * Client-side connection for in game.  Handles forwarding messages to bases and unit groups.
 */
class ClientPlayerConnection(val socketData:SocketInfoData, mapReadyCallback:() => Unit) extends Runnable{

	def in = socketData.in
	def out = socketData.out
	
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
		running = true
		
		//read map and initialize
		val gameMapProto = GameProto.Map.parseDelimitedFrom(in)
		gameMap = protocolToMap(gameMapProto, out)
		
		//tell callback map is ready
		mapReadyCallback()
		
		//get player, then do the game loop
		val player = PlayerProto.Player.parseDelimitedFrom(in)
		this.pData = Some(new Player(player.getName(), player.getId(), new Color(player.getColor())))
		basicLoop()
		running = false
	}
	private var continue = true
	def basicLoop(){
		while(continue){
		try{
			val msg = GameProto.IncomingMessageType.parseDelimitedFrom(in).getType()
			msg match{
				case UNIT_ARRIVAL =>
					val changeMsg = GameProto.UnitArrival.parseDelimitedFrom(in)
					val changeData = changeMsg.getArrived()
					val baseObj = map.objectAt(changeMsg getX, changeMsg getY)
					val player = map.players find {_.id == changeData.getOwner()}
					player foreach {
						owner =>
							baseObj foreach {_.unitArrival(changeData.getCount(), owner.id)}
					}
				case OWNER_CHANGE =>
					val changeMsg = GameProto.OwnerChange.parseDelimitedFrom(in)
					val newOwner = map.players find {_.id == changeMsg.getNewOwnerId()}
					val baseObj = map.objectAt(changeMsg getX, changeMsg getY)
						newOwner foreach {
						owner =>
							baseObj foreach {_.setOwner(owner.id)}
					}
				case ALL_UNIT_CHANGE =>
					val changeMsg = GameProto.AllUnitChange.parseDelimitedFrom(in)
					val baseObj = map.objectAt(changeMsg.getX(), changeMsg.getY())
					val unitMap = unitInfoListProtoToBaseList(changeMsg.getUnitInfoList(), map.players)
					baseObj foreach {
						obj =>
							obj.setAllUnits(unitMap)
					}
				case PLAYER_DATA =>
					val playerData = PlayerProto.Player.parseDelimitedFrom(in)
					this.pData = Some(new Player(playerData.getName(), playerData.getId(), new Color(playerData.getColor())))
				case NEW_UNIT_GROUP =>
					val ugData = GameProto.NewUnitGroup.parseDelimitedFrom(in)
					val group = new UnitGroup(ugData.getId(), ugData.getOwner(), ugData.getCount(), BaseObject.genBasicUnitSprite(map), map, null, null)
					group.setLocation(Location(ugData.getX(), ugData.getY()))
					map.addUnitGroup(group)
				case UNIT_GROUP_POSITION =>
					val ugp = GameProto.UnitGroupPosition.parseDelimitedFrom(in)
					if(ugp.getComplete()){
						//if the group has finished moving remove it
						map.removeGroup(ugp.getId())
					}else{
						//otherwise update
						map.groupForId(ugp.getId()) foreach {
							ug =>
								val loc = Location(ugp.getX(), ugp.getY())
								ug.setLocation(loc)
						}						
					}
				case DISCONNECT =>
					continue = false
				case LEAVE_GAME =>
					continue = false
					val conf = GameProto.IncomingMessageType.newBuilder().setType(CONFIRM_LEAVE_GAME).build()
					out.write(conf)
					new Thread(new WormholeMainClient(socketData)).start()
				case CONFIRM_LEAVE_GAME =>
					continue = false
					new Thread(new WormholeMainClient(socketData), "CPC MainClient").start()
				case PLAYER_VICTORY =>
					val winner = GameProto.Victory.parseDelimitedFrom(in).getWinnerId()
					//TODO improve victory/defeat display
					JOptionPane.showMessageDialog(null, if(playerData exists {_.id==winner}) "Victory" else "Defeat")
				case _ =>
			}
			}catch{
				case e:IOException =>
					//TODO log exception
					continue = false
			}
		}
	}
	def leaveGame(){
		val msg = GameProto.IncomingMessageType.newBuilder().setType(LEAVE_GAME).build()
		out.write(msg)
	}
	def disconnect(){
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.DISCONNECT).build()
		continue = false
		out.write(msgType)
	}
}
