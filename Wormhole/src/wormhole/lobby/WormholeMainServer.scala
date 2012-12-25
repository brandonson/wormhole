package wormhole.lobby

import akka.actor.Actor
import java.net.ServerSocket
import java.io.InterruptedIOException
import java.io.IOException
import java.net.Socket
import wormhole.SocketInfoData
import wormhole.WormholeSystem
import wormhole.actor._
import akka.actor.Props
import akka.pattern.ask
import wormhole.lobby.network.MainScreenProto
import org.slf4j.LoggerFactory
import akka.actor.ActorLogging

/**
 * Main server class for Wormhole Server.  Handles accepting connections,
 * then hands them off to a WormholeClientHandler.
 */
class WormholeMainServer(port:Int) extends Runnable{

	val log = LoggerFactory.getLogger("WormholeMainServer")
  
	private[this] var continue = true
	private[this] val ref = WormholeSystem.actorOf(Props(new WormholeMainServerImpl(this)))
	
	def run{
		log info "Starting main server"
		val server = new ServerSocket(port)
		while(!Thread.interrupted()&&continue){
			try{
				log debug "Accepted incoming exception"
				handleNewConnection(new SocketInfoData(server.accept()))
			}catch{
				case exc:InterruptedIOException =>
				  log.warn("Interrupted during execution: server stopped", exc)
				case ioe:IOException =>
				  	log.warn("Exception while waiting to accept socket", ioe)
					continue = false
			}
		}
	}
	
	def handleNewConnection(socketInfo:SocketInfoData){
		val ch = new WormholeClientHandler(socketInfo)
		new Thread(ch, "ClientHandler").start()
		ref ! ch
		log debug "ClientHandler started and recorded"
	}
	def disconnectConn(wch:WormholeClientHandler){
		ref ! ('Disconnect, wch)
	}
	def disconnectAll{
		ref ! 'DoDisconnect
	}
	
	def newLobby(lobbyName:String):WormholeServerLobby = {
		fetch((ref ? ('NewLobby, lobbyName)).mapTo[WormholeServerLobby])
	}
	def lobbyDropped(lobbyId:Int) = {
		ref ! ('LobbyDropped, lobbyId)
	}
	def lobbyList = {
		fetch((ref ? 'LobbyList).mapTo[List[WormholeServerLobby]])
	}
	def lobbyForId(id:Int) = {
		fetch((ref ? ('LobbyForId, id)).mapTo[Option[WormholeServerLobby]])
	}
}

class WormholeMainServerImpl(val mserver:WormholeMainServer) extends Actor with ActorLogging{
	var connections:List[WormholeClientHandler] = Nil
	var lobbies:List[WormholeServerLobby] = Nil
	var lobbyId = 0
	def receive = {
		case 'DoDisconnect =>
		  	log info "Disconnecting all clients"
			connections foreach {_.disconnect}
		case newConn:WormholeClientHandler =>
		  	log info "New connection"
			connections ::= newConn
		case ('Disconnect, wch:WormholeClientHandler) =>
		  	log info "Client disconnected"
			connections = connections filterNot{_ == wch}
		case ('NewLobby, lobbyName:String) =>
		  	log info ("New lobby " + lobbyName + " created")
			lobbyId += 1
			val lobby = new WormholeServerLobby(lobbyName, lobbyId, mserver)
			lobbies ::= lobby
			val dataBuild = MainScreenProto.LobbyData.newBuilder()
			dataBuild.setId(lobbyId)
			dataBuild.setName(lobbyName)
			dataBuild.build()
			val data = dataBuild.build()
			connections foreach {_.lobbyAdded(data)}
		  	log debug "Told connections about new lobby"
			sender ! lobby
			
		case ('LobbyDropped, lobbyId:Int) =>
		  	log info "Lobby removed"
			lobbies = lobbies filterNot {_.id == lobbyId}
			connections foreach {_.lobbyRemoved(lobbyId)}
		case 'LobbyList =>
			sender ! lobbies
		case ('LobbyForId, id:Int) =>
			sender ! (lobbies find {_.id == id})
	}
}