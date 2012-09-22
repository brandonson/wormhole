import akka.actor.ActorSystem
package object wormhole {
	
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
}
