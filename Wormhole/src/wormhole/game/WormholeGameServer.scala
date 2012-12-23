package wormhole.game

import java.awt.Color
import java.net.ServerSocket
import wormhole.game.network.ServerPlayerConnection
import java.util.Timer
import wormhole.RunnableConversion._
import wormhole.SocketInfoData
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.TimerTask
import wormhole.Player
import org.slf4j.LoggerFactory

/**
 * Game server class.  Stores the map and list of connections.
 */
class WormholeGameServer(val map:WormholeMap, playerData:List[(SocketInfoData,Player)]) {

	val log = LoggerFactory.getLogger("WormholeGameServer")
  
	private[this] lazy val connections = {
	  log.debug("Generating ServerPlayerConnections for game")
	  playerData map {
		tup =>
			new ServerPlayerConnection(tup._2, map, tup._1)
	  }
	}
	private[this] val task:TimerTask = map.updateAll _
	/**
	 * Starts the game.  Schedules the TimerTask to run and starts the connection handler threads.
	 */
	def start(){
		log.debug("Starting game")
		connections.zipWithIndex foreach {tup => new Thread(tup._1, "SPC-"+tup._2).start()}
		val timer = new Timer()
		timer.scheduleAtFixedRate(task, 50, 50)
		log.debug("Game started, TimerTask running")
	}
	def stop(){
		log.debug("Game stopped")
		task.cancel()
	}
}
