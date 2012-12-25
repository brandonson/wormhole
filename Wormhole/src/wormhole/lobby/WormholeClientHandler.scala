package wormhole.lobby

import wormhole.SocketInfoData
import wormhole.lobby.network.MainScreenProto
import wormhole.lobby.network.MainScreenProto.MessageType._
import akka.pattern.AskTimeoutException
import scala.collection.JavaConversions._
import java.io.IOException
import wormhole.WormholeServer.mainServer
import org.slf4j.LoggerFactory

/**
 * Server-side handler for clients at the main screen.
 */
class WormholeClientHandler(val socket:SocketInfoData) extends Runnable{

	
	val log = LoggerFactory.getLogger("WormholeClientHandler")
	
	def in = socket.in
	def out = socket.out
	
	def disconnect{
		log info "Server ordered disconnect from client"
		out.write(MainScreenProto.MainMessageType.newBuilder().setType(DISCONNECT).build())
	}
	private[this] var continue = true
	
	/**
	 * Sends the list of lobbies to the client.
	 */
	def writeLobbyList(){
		log debug "Sending lobby list"
		val list = mainServer.lobbyList
		val ldata = list map {
			lobby =>
				val builder = MainScreenProto.LobbyData.newBuilder()
				builder.setId(lobby.id).setName(lobby.name)
				builder.build()
		}
		log debug "Lobby list prepared"
		val listMsg = MainScreenProto.LobbyDataList.newBuilder()
		listMsg.addAllLobby(ldata)
		log debug "Message ready"
		out.write(listMsg.build())
	}
	
	def run(){
		log info "Starting client handler"
		writeLobbyList()
		log info "Initial lobby list sent"
		while(!Thread.interrupted()&&continue){
			try{
				val incomingType = MainScreenProto.MainMessageType.parseDelimitedFrom(in)
				incomingType.getType() match {
					case CREATE_LOBBY =>
					  	log debug "Received CREATE_LOBBY message"
						val createMessage = MainScreenProto.CreateLobby.parseDelimitedFrom(in)
						log info "Read CREATE_LOBBY message data"
						val lobby = mainServer.newLobby(createMessage.getName())
						val connected = lobby.addConnection(socket)
						if(connected){
							continue = false
						}
					case JOIN_LOBBY =>
					  	log debug "Received JOIN_LOBBY message"
						val joinMsg = MainScreenProto.LobbyIdMessage.parseDelimitedFrom(in)
						log info "Read JOIN_LOBBY message data"
						
						//join the given lobby if possible
						val forId = mainServer.lobbyForId(joinMsg.getLobbyId())
						val joined = forId map {
							lobby =>
								try{
									log debug "Joining lobby"
									//lobby notifies user, we don't have to
									lobby.addConnection(socket)
								}catch{
									case LobbyFullException =>
										false
								}
						}
						//if we joined the lobby, stop, otherwise do nothing
						if(joined getOrElse false){
							log debug "Lobby successfully joined, stopping client handler"
							continue = false
						}
					case DISCONNECT =>
					  	log info "Received DISCONNECT message"
						socket.socket.close()
						continue = false
				}
			}catch{
				case e:IOException =>
				  	log.warn("IOException while reading message or message data.", e)
					continue = false
			}
		}
		mainServer.disconnectConn(this)
		log info "Disconnecting client"
	}
	
	def lobbyAdded(data:MainScreenProto.LobbyData){
		log info "New lobby created, informing client"
		val mType = MainScreenProto.MainMessageType.newBuilder().setType(NEW_LOBBY).build()
		out.write(mType, data)
	}
	
	def lobbyRemoved(id:Int){
		log info "Lobby removed, informing client"
		val mType = MainScreenProto.MainMessageType.newBuilder().setType(REMOVED_LOBBY).build()
		val data = MainScreenProto.LobbyIdMessage.newBuilder().setLobbyId(id).build()
		out.write(mType, data)
	}
}