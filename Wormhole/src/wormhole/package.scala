import akka.actor.ActorSystem
import com.wormhole.network.PlayerProto
import java.awt.Color
package object wormhole {
	
	type PlayerId = Int
	/**
	 * The port wormhole uses.
	 */
	val CONNECTION_PORT = 25464
	/**
	 * Main ActorSystem, used for all actors.
	 */
	val WormholeSystem = ActorSystem()
	/**
	 * The connection from a client to the server.  Is null when
	 * running a server.
	 */
	var clientConnection:ThreadsafeMessageWriter = null
	
	implicit def protoToPlayer(conv:PlayerProto.Player) = new Player(conv.getName(), conv.getId(), new Color(conv.getColor()))
}
