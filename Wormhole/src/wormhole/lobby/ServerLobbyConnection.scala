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
/**
 * Server-side handler for single client connections to a game lobby.
 */
class ServerLobbyConnection(val data:SocketInfoData, val lobby:WormholeServerLobby, ownData:PlayerProto.Player) extends Runnable{

	def in = data.in
	def out = data.out
	

	private var waitingGame:WormholeGameServer = null
	
	def run(){
		//TODO move notification that the user is in a lobby to WormholeClientHandler
		//could have concurrency issues
		out write genJoinLobbyMessage	//inform client we are in a lobby
		out write WormholeServerLobby.possibleColorMessage
		out write ownData
		out write genPersonSetMessage
		try{
			basicLoop()
		}catch{
			case ioe:IOException =>
				data.socket.close()
		}
		lobby.lostPlayer(this)
	}
	def basicLoop(){
		var continue = true
		while(continue){
			LobbyProto.LobbyMessageType.parseDelimitedFrom(in).getType() match {
				case PERSON_SET_INFO =>
					val mType = LobbyProto.LobbyMessageType.newBuilder().setType(PERSON_SET_INFO).build()
					val count = genPersonSetMessage
					out.write(mType, count)
				case CHANGE_INFO =>
					val change = PlayerProto.Player.parseDelimitedFrom(in)
					lobby.dataChanged(this, change)
				case START =>
					lobby.start()
				case START_CONFIRM =>
					continue = false
				case DISCONNECT =>
					continue = false
				case RETURN_TO_MAIN =>
					continue = false
					val msg = LobbyProto.LobbyMessageType.newBuilder().setType(CONFIRM_RETURN_TO_MAIN).build()
					out.write(msg)
					lobby.mainServer.handleNewConnection(data)
			}
		}
	}
	def genPersonSetMessage = LobbyProto.PersonSetInfo.newBuilder().addAllInfo(JavaConversions.asJavaIterable(lobby.personInfoSet)).build()
	def genJoinLobbyMessage = MainScreenProto.MainMessageType.newBuilder().setType(MainScreenProto.MessageType.JOIN_LOBBY).build()
	def start(game:WormholeGameServer){
		waitingGame = game
		val mType = LobbyProto.LobbyMessageType.newBuilder().setType(START).build()
		out.write(mType)
	}
	def lostPerson(info:PlayerProto.Player){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(LOST_PERSON).build()
		out write (msg, info)
	}
	def newPerson(info:PlayerProto.Player){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(NEW_PERSON).build()
		out write (msg, info)
	}
	def infoChanged(old:PlayerProto.Player, newInf:PlayerProto.Player){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
		out write(msg, old, newInf)
	}
}