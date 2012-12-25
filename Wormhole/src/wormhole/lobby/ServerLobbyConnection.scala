package wormhole.lobby

import wormhole.SocketInfoData
import wormhole.lobby.network.LobbyProto
import wormhole.lobby.network.LobbyProto.MessageType._
import wormhole.game.WormholeGameServer
import wormhole.game.network.ServerPlayerConnection
import java.io.IOException
import java.awt.Color
import scala.collection.JavaConversions
import wormhole.lobby.network.MainScreenProto
import com.wormhole.network.PlayerProto
import org.slf4j.LoggerFactory
/**
 * Server-side handler for single client connections to a game lobby.
 */
class ServerLobbyConnection(val data:SocketInfoData, val lobby:WormholeServerLobby, ownData:PlayerProto.Player) extends Runnable{

	val log = LoggerFactory.getLogger("ServerLobbyConnection")
  
	def in = data.in
	def out = data.out
	

	private var waitingGame:WormholeGameServer = null
	
	def run(){
		log debug "Sending initialization data"
		//TODO move notification that the user is in a lobby to WormholeClientHandler
		//could have concurrency issues
		out write genJoinLobbyMessage	//inform client we are in a lobby
		out write WormholeServerLobby.possibleColorMessage
		out write ownData
		out write genPersonSetMessage
		log info "Initialization data sent"
		try{
			basicLoop()
		}catch{
			case ioe:IOException =>
				data.socket.close()
		}
		lobby.lostPlayer(this)
		log.info("Removed from lobby")
	}
	def basicLoop(){
		var continue = true
		while(continue){
			LobbyProto.LobbyMessageType.parseDelimitedFrom(in).getType() match {
				case PERSON_SET_INFO =>
					log debug "Received PERSON_SET_INFO message"
				  	val mType = LobbyProto.LobbyMessageType.newBuilder().setType(PERSON_SET_INFO).build()
					log info "Read PERSON_SET_INFO message data"
				  	val count = genPersonSetMessage
					out.write(mType, count)
				case CHANGE_INFO =>
					log debug "Received CHANGE_INFO message"
					val change = PlayerProto.Player.parseDelimitedFrom(in)
					log info "Read CHANGE_INFO message data"
					lobby.dataChanged(this, change)
				case START =>
					log info "Received START message"
					lobby.start()
				case START_CONFIRM =>
					log info "Received START_CONFIRM message"
					continue = false
				case DISCONNECT =>
					log info "Received DISCONNECT message"
					continue = false
				case RETURN_TO_MAIN =>
					log info "Received RETURN_TO_MAIN message"
					continue = false
					val msg = LobbyProto.LobbyMessageType.newBuilder().setType(CONFIRM_RETURN_TO_MAIN).build()
					out.write(msg)
					log debug "Sent CONFIRM_RETURN_TO_MAIN"
					lobby.mainServer.handleNewConnection(data)
					log debug "Returned connection to control of main server"
			}
		}
	}
	def genPersonSetMessage = LobbyProto.PersonSetInfo.newBuilder().addAllInfo(JavaConversions.asJavaIterable(lobby.personInfoSet)).build()
	def genJoinLobbyMessage = MainScreenProto.MainMessageType.newBuilder().setType(MainScreenProto.MessageType.JOIN_LOBBY).build()
	def start(game:WormholeGameServer){
		log info "Game started, informing client"
		waitingGame = game
		val mType = LobbyProto.LobbyMessageType.newBuilder().setType(START).build()
		out.write(mType)
	}
	def lostPerson(info:PlayerProto.Player){
		log info "Person left, informing client"
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(LOST_PERSON).build()
		out write (msg, info)
	}
	def newPerson(info:PlayerProto.Player){
		log info "New person, informing client"
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(NEW_PERSON).build()
		out write (msg, info)
	}
	def infoChanged(old:PlayerProto.Player, newInf:PlayerProto.Player){
		log info "Person info changed, informing client"
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
		out write(msg, old, newInf)
	}
}