package wormhole.lobby

import wormhole.SocketInfoData
import wormhole.lobby.network.MainScreenProto
import wormhole.lobby.network.MainScreenProto.MessageType._
import akka.pattern.AskTimeoutException
import scala.collection.JavaConversions._
import java.io.IOException

/**
 * Server-side handler for clients at the main screen.
 */
class WormholeClientHandler(val socket:SocketInfoData) extends Runnable{

	import wormhole.WormholeServer.mainServer
	
	def in = socket.in
	def out = socket.out
	
	def disconnect{
		out.write(MainScreenProto.MainMessageType.newBuilder().setType(DISCONNECT).build())
	}
	private[this] var continue = true
	
	/**
	 * Sends the list of lobbies to the client.
	 */
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
			try{
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
						//join the given lobby if possible
						val joinMsg = MainScreenProto.LobbyIdMessage.parseDelimitedFrom(in)
						val forId = mainServer.lobbyForId(joinMsg.getLobbyId())
						val joined = forId map {
							lobby =>
								try{
									//lobby notifies user, we don't have to
									lobby.addConnection(socket)
								}catch{
									case LobbyFullException =>
										false
								}
						}
						//if we joined the lobby, stop, otherwise do nothing
						if(joined getOrElse false){
							continue = false
						}
					case DISCONNECT =>
						socket.socket.close()
						continue = false
				}
			}catch{
				case e:IOException =>
					continue = false
			}
		}
		mainServer.disconnectConn(this)
	}
	
	def lobbyAdded(data:MainScreenProto.LobbyData){
		val mType = MainScreenProto.MainMessageType.newBuilder().setType(NEW_LOBBY).build()
		out.write(mType, data)
	}
	def lobbyRemoved(id:Int){
		val mType = MainScreenProto.MainMessageType.newBuilder().setType(REMOVED_LOBBY).build()
		val data = MainScreenProto.LobbyIdMessage.newBuilder().setLobbyId(id).build()
		out.write(mType, data)
	}
}