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

class WormholeMainServer(port:Int) extends Runnable{

	private[this] var continue = true
	private[this] val ref = WormholeSystem.actorOf(Props(classOf[WormholeMainServerImpl]))
	
	def run{
		val server = new ServerSocket(port)
		while(!Thread.interrupted()&&continue){
			try{
				handleNewConnection(server.accept())
			}catch{
				case _:InterruptedIOException =>
				case _:IOException =>
					continue = false
			}
		}
	}
	
	def handleNewConnection(socket:Socket){
		val socketInfo = new SocketInfoData(socket)
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

class WormholeMainServerImpl(val mserver:WormholeMainServer) extends Actor{
	var connections:List[WormholeClientHandler] = Nil
	var lobbies:List[WormholeServerLobby] = Nil
	var lobbyId = 0
	def receive = {
		case 'DoDisconnect =>
			connections foreach {_.disconnect}
		case newConn:WormholeClientHandler =>
			connections ::= newConn
		case ('Disconnect, wch:WormholeClientHandler) =>
			connections -= wch
		case ('NewLobby, lobbyName:String) => 
			lobbyId += 1
			val lobby = new WormholeServerLobby(lobbyName, lobbyId, mserver)
			lobbies ::= lobby
			sender ! lobby
		case ('LobbyDropped, lobbyId:Int) =>
			lobbies = lobbies filterNot {_.id == lobbyId}
		case 'LobbyList =>
			sender ! lobbies
		case ('LobbyForId, id:Int) =>
			sender ! (lobbies find {_.id == id})
	}
}