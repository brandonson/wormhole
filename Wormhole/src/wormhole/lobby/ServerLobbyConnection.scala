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
class ServerLobbyConnection(val data:SocketInfoData, val lobby:WormholeServerLobby, ownData:LobbyProto.PersonInfo) extends Runnable{

	def in = data.in
	def out = data.out
	out write genJoinLobbyMessage	//inform client we are in a lobby
	out write WormholeServerLobby.possibleColorMessage
	out write ownData
	out write genPersonSetMessage
	private var waitingGame:WormholeGameServer = null
	
	def run(){
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
					val change = LobbyProto.PersonInfo.parseDelimitedFrom(in)
					lobby.dataChanged(this, change)
				case START =>
					lobby.start()
				case START_CONFIRM =>
					continue = false
				case DISCONNECT =>
					continue = false
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
	def lostPerson(info:LobbyProto.PersonInfo){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(LOST_PERSON).build()
		out write (msg, info)
	}
	def newPerson(info:LobbyProto.PersonInfo){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(NEW_PERSON).build()
		out write (msg, info)
	}
	def infoChanged(old:LobbyProto.PersonInfo, newInf:LobbyProto.PersonInfo){
		val msg = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
		out write(msg, old, newInf)
	}
}