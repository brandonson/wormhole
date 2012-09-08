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
class ClientPlayerConnection(val socketData:SocketInfoData, mapReadyCallback:() => Unit) extends Runnable{

	def in = socketData.in
	def out = socketData.out
	
	private var gameMap:WormholeMap = null
	
	def map = gameMap
	
	private var pData:Option[Player] = None
	
	def playerData = pData
	
	private var running = false
	
	def isRunning = running
	def run(){
		running = true
		val gameMapProto = GameProto.Map.parseDelimitedFrom(in)
		gameMap = protocolToMap(gameMapProto, out)
		mapReadyCallback()
		val player = GameProto.Player.parseDelimitedFrom(in)
		this.pData = Some(new Player(player.getId(), new Color(player.getColor())))
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
					val playerData = GameProto.Player.parseDelimitedFrom(in)
					this.pData = Some(new Player(playerData.getId(), new Color(playerData.getColor())))
				case DISCONNECT =>
					continue = false
				case _ =>
			}
			}catch{
				case e:IOException =>
					continue = false
			}
		}
	}
	def disconnect(){
		val msgType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.DISCONNECT).build()
		continue = false
		out.write(msgType)
	}
}