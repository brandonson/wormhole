package wormhole.lobby
import java.net.Socket
import wormhole.lobby.network.LobbyProto.MessageType._
import wormhole.SocketInfoData
import wormhole.lobby.network.LobbyProto
import akka.actor.Actor
import wormhole.WormholeSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.pattern.ask
import wormhole.actor._
import wormhole.RunnableConversion._
import java.awt.Color
import wormhole.game.WormholeGameServer
import scala.collection.JavaConversions._
import wormhole.game.MapUtils
import wormhole.Player
import wormhole.WormholeServer
import com.wormhole.network.PlayerProto
import akka.actor.ActorLogging
object WormholeServerLobby{
	val possibleColors = List(new Color(255,0,0), new Color(0,255,0), new Color(0,0,255), new Color(255,255,0), new Color(0,255,255), new Color(255,0,255),
			new Color(150,60,215), new Color(255,150,0), new Color(240,200, 225), new Color(120,145,15))

	val possibleColorMessage = {
		val scalaList = possibleColors map {col => LobbyProto.ColorData.newBuilder().setColor(col.getRGB()).build()}
		LobbyProto.PossibleColorList.newBuilder().addAllColors(scalaList).build()
	}
}

class WormholeServerLobby(val name:String, val id:Int, val mainServer:WormholeMainServer) {

	val ref = WormholeSystem.actorOf(Props(new WormholeServerLobbyImpl(this)))
	
	def addConnection(data:SocketInfoData) = {
		fetch((ref ? ('NewConnection, data)).mapTo[Boolean])
	}
	
	def start(){ ref ! 'Start}
	
	def personInfoSet:List[PlayerProto.Player] = fetch(personInfoSetFuture)
	def personInfoSetFuture = (ref ? 'PersonSet).mapTo[List[PlayerProto.Player]]
	
	def lostPlayer(conn:ServerLobbyConnection){
		ref ! ('Lost, conn)
	}
	
	def dataChanged(conn:ServerLobbyConnection, newInfo:PlayerProto.Player){
		ref ! ('InfoChange, conn, newInfo)
	}
}

private class WormholeServerLobbyImpl(val lobby:WormholeServerLobby) extends Actor with ActorLogging{
	
	import WormholeServerLobby._
	import WormholeServer._
	var connections:List[(ServerLobbyConnection,Thread, PlayerProto.Player)] = Nil
	var availableColors:List[Int] = possibleColors map {_.getRGB()}
	var active = true;
	var idGen = 0
	
	private[this] def newColor() = {
		val opt = availableColors.headOption
		availableColors = availableColors.tail
		opt
	}
	
	private[this] def nextId():Int = {
		idGen += 1
		idGen
	}
	
	def receive = {
		case ('NewConnection, data:SocketInfoData) =>
			if(active){
				log info "Adding new connection"
				val result = newColor map {
					color =>
					val id = nextId
					val info = PlayerProto.Player.newBuilder().setName("unknown").setId(id).setColor(color).build()
					val conn = new ServerLobbyConnection(data, lobby, info)
					val thread = new Thread(conn, "SLC-" + connections.size)
					thread.start()
					log debug "Started connection handler"
					connections foreach {_._1.newPerson(info)}
					connections ::= (conn,thread,info)
				}
				val resBool:Boolean = result.isDefined
				sender ! resBool
			}else{
				sender ! false
			}
		case 'PersonSet =>
			sender ! (connections map {_._3})
		case ('InfoChange, conn:ServerLobbyConnection, newInfo:PlayerProto.Player) =>
			log info "Info changed for a connection"
			val original = connections find {_._1==conn}
			original foreach {
				orig =>
					connections = connections filterNot {_==orig}
					connections ::= (orig._1, orig._2, newInfo)
					availableColors ::= orig._3.getColor()
					availableColors = availableColors filterNot {_==newInfo.getColor()}
					connections foreach {_._1.infoChanged(orig._3, newInfo)}
					log debug "Person " + orig._3.getName() + " changed to " + newInfo.getName()
			}
		case ('Lost, conn:ServerLobbyConnection) =>
		  	log info "Connection lost"
			val data = connections.find {_._1 == conn} map {_._3}
			connections = connections.filterNot(_._1 == conn)
			data foreach {person => 
				connections foreach {_._1.lostPerson(person)}
				availableColors ::= person.getColor()
			}
			log debug "Informed other connections of disconnect"
			if(connections.length==0){
				active = false
				lobby.mainServer.lobbyDropped(lobby.id)
				context.stop(self)
				log info "Stopping because no connections are active"
			}
		case 'Start =>
		  	log info "Starting game"
			val players = (connections.map {_._3}).zipWithIndex map {
				tup =>
					val (pInf, idx) = tup
					new Player(pInf.getName(), idx, new Color(pInf.getColor()))
			}
			val zipped = connections map {_._1.data} zip players
			val map = MapUtils.genRandomMap(MapWidth, MapHeight, PlanetCount, MaxProduction+1, MaxDefense+1, players)
			val server = new WormholeGameServer(map, zipped)
			val conns = connections
			connections foreach {_._1.start(server)}
			log debug "Told connections to start"
			availableColors = connections.map {_._3.getColor()} ++ availableColors
			connections = Nil
			joinConnectionsAndStart(conns map {_._2}, server)
			log debug "Game has started"
			lobby.mainServer.lobbyDropped(lobby.id)
			context.stop(self)
			log debug "Lobby removed from server, stopping operation"
	}
	
	def joinConnectionsAndStart(list:List[Thread], server:WormholeGameServer){
		list foreach {_.join()}
		server.start()
	}
}