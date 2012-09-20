package wormhole

import wormhole.lobby.WormholeServerLobby
import java.net.ServerSocket
import wormhole.lobby.LobbyFullException
import wormhole.lobby.WormholeMainServer

object WormholeServer {

	var mainServer:WormholeMainServer = null
	
	val MapWidth = 10
	val MapHeight = 10
	val PlanetCount = 20
	val MaxProduction = 10
	val MaxDefense = 0
	
	def main(args:Array[String]){
		mainServer = new WormholeMainServer(CONNECTION_PORT)
		new Thread (mainServer).run()
	}
}