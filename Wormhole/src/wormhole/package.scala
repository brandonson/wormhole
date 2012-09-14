import akka.actor.ActorSystem
package object wormhole {
	

	val CONNECTION_PORT = 25464
	val WormholeSystem = ActorSystem()
	var clientConnection:ThreadsafeMessageWriter = null
}
