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
import wormhole.game.Player
import wormhole.WormholeServer
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
		val color = fetch((ref?'NewColor).mapTo[Option[Int]]).getOrElse(throw LobbyFullException)
		val info = LobbyProto.PersonInfo.newBuilder().setName("unknown").setColor(color).build()
		val conn = new ServerLobbyConnection(data, this, info)
		fetch((ref ? ('NewConnection, conn,info)).mapTo[Boolean])
	}
	
	def start(){ ref ! 'Start}
	
	def personInfoSet:List[LobbyProto.PersonInfo] = fetch(personInfoSetFuture)
	def personInfoSetFuture = (ref ? 'PersonSet).mapTo[List[LobbyProto.PersonInfo]]
	
	def lostPlayer(conn:ServerLobbyConnection){
		ref ! ('Lost, conn)
	}
	
	def dataChanged(conn:ServerLobbyConnection, newInfo:LobbyProto.PersonInfo){
		ref ! ('InfoChange, conn, newInfo)
	}
}

private class WormholeServerLobbyImpl(val lobby:WormholeServerLobby) extends Actor{
	
	import WormholeServerLobby._
	import WormholeServer._
	var connections:List[(ServerLobbyConnection,Thread, LobbyProto.PersonInfo)] = Nil
	var availableColors:List[Int] = possibleColors map {_.getRGB()}
	var active = true;
	def receive = {
		case ('NewConnection, ref:ServerLobbyConnection, info:LobbyProto.PersonInfo) =>
			if(active){
				val thread = new Thread(ref, "SLC-" + connections.size)
				thread.start()
				connections ::= (ref,thread,info)
				availableColors -= info.getColor()
				connections foreach {_._1.newPerson(info)}
				true
			}else false
		case 'NewColor =>
			sender ! (availableColors.headOption)
		case 'PersonSet =>
			sender ! (connections map {_._3})
		case ('InfoChange, conn:ServerLobbyConnection, newInfo:LobbyProto.PersonInfo) =>
			val original = connections find {_._1==conn}
			original foreach {
				orig => 
					connections -= orig;
					connections ::= (orig._1, orig._2, newInfo)
					availableColors ::= orig._3.getColor()
					availableColors -= newInfo.getColor()
					connections foreach {_._1.infoChanged(orig._3, newInfo)}
			}
		case ('Lost, conn:ServerLobbyConnection) =>
			val data = connections.find {_._1 == conn} map {_._3}
			data foreach {person => 
				connections foreach {_._1.lostPerson(person)}
				availableColors ::= person.getColor()
			}
			connections = connections.filterNot(_._1 == conn)
			if(connections.length==0){
				active = false
				lobby.mainServer.lobbyDropped(lobby.id)
			}
		case 'Start =>
			val players = (connections.map {_._3}).zipWithIndex map {
				tup =>
					val (pInf, idx) = tup
					new Player(idx, new Color(pInf.getColor()))
			}
			val zipped = connections map {_._1.data} zip players
			val map = MapUtils.genRandomMap(MapWidth, MapHeight, PlanetCount, MaxProduction+1, MaxDefense+1, players)
			val server = new WormholeGameServer(map, zipped)
			val conns = connections
			connections foreach {_._1.start(server)}
			availableColors = connections.map {_._3.getColor()} ++ availableColors
			connections = Nil
			new Thread(() => joinConnectionsAndStart(conns map {_._2}, server)).start()
	}
	
	def joinConnectionsAndStart(list:List[Thread], server:WormholeGameServer){
		list foreach {_.join()}
		server.start()
	}
}