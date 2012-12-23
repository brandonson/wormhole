package wormhole

import akka.actor.Actor
import akka.actor.Props
import wormhole.actor._
import akka.pattern.ask
import akka.actor.ActorLogging

class IdProvider {

	private[this] val ref = WormholeSystem.actorOf(Props(classOf[IdProviderImpl]))
	
	def newIdFuture = (ref ? 'NewId).mapTo[Int]
	def newId = fetch(newIdFuture)
}

private class IdProviderImpl extends Actor with ActorLogging{
	var id = 0;
	
	def receive = {
		case 'NewId =>
			log.debug("Granting new id: " + id)
			sender ! id
			id += 1
	}
}