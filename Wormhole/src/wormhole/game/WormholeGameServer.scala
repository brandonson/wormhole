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

class WormholeGameServer(val map:WormholeMap, playerData:List[(SocketInfoData,Player)]) {
	
	val connections = playerData map {
		tup =>
			new ServerPlayerConnection(tup._2, map, tup._1)
	}
	val task:TimerTask = map.updateAll _
	def start(){
		connections.zipWithIndex foreach {tup => new Thread(tup._1, "SPC-"+tup._2).start()}
		val timer = new Timer()
		timer.scheduleAtFixedRate(task, 500, 500)
	}
	def stop(){
		task.cancel()
	}
}