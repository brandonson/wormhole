package wormhole

import wormhole.lobby.WormholeServerLobby
import java.net.ServerSocket
import wormhole.lobby.LobbyFullException
import wormhole.lobby.WormholeMainServer

/**
 * Main class for servers. Also stores constants.  Note that many constants are used in map generation, and
 * will be removed when the lobby allows game customization.
 */
object WormholeServer {

	/**
	 * The server instance.
	 */
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