package wormhole.lobby

import wormhole.SocketInfoData
import wormhole.lobby.network.MainScreenProto
import wormhole.lobby.network.MainScreenProto.MessageType._
import akka.pattern.AskTimeoutException
import scala.collection.JavaConversions._

class WormholeClientHandler(val socket:SocketInfoData, val mainServer:WormholeMainServer) extends Runnable{

	def in = socket.in
	def out = socket.out
	
	def disconnect{
		out.write(MainScreenProto.MainMessageType.newBuilder().setType(DISCONNECT).build())
	}
	private[this] var continue = true
	
	def writeLobbyList(){
		val list = mainServer.lobbyList
		val ldata = list map {
			lobby =>
				val builder = MainScreenProto.LobbyData.newBuilder()
				builder.setId(lobby.id).setName(lobby.name)
				builder.build()
		}
		val listMsg = MainScreenProto.LobbyDataList.newBuilder()
		listMsg.addAllLobby(ldata)
		out.write(listMsg.build())
	}
	
	def run(){
		writeLobbyList()
		while(!Thread.interrupted()&&continue){
			val incomingType = MainScreenProto.MainMessageType.parseDelimitedFrom(in)
			incomingType.getType() match {
				case CREATE_LOBBY =>
					val createMessage = MainScreenProto.CreateLobby.parseDelimitedFrom(in)
					val lobby = mainServer.newLobby(createMessage.getName())
					val connected = lobby.addConnection(socket)
					if(connected){
						continue = false
					}
				case JOIN_LOBBY =>
					val joinMsg = MainScreenProto.LobbyIdMessage.parseDelimitedFrom(in)
					val forId = mainServer.lobbyForId(joinMsg.getLobbyId())
					val joined = forId map {_.addConnection(socket)}
					if(joined getOrElse false){
						continue = false
					}
				case DISCONNECT =>
					mainServer.disconnectConn(this)
					socket.socket.close()
			}
		}
	}
}