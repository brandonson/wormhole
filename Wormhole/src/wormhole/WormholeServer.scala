package wormhole

import wormhole.lobby.WormholeServerLobby
import java.net.ServerSocket
import wormhole.lobby.LobbyFullException

object WormholeServer {

	def main(args:Array[String]){
		var lobby = new WormholeServerLobby()
		val server = new ServerSocket(CONNECTION_PORT)
		while(true){
			val sock = server.accept()
			try{
				lobby.addConnection(new SocketInfoData(sock))
			}catch{
				case LobbyFullException =>
					lobby = new WormholeServerLobby()
			}
		}
	}
}