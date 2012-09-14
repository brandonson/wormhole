package wormhole.game

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import java.awt.Color
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import akka.util.duration._
import wormhole.actor._
import wormhole.graphics.PlanetSprite
import wormhole.ThreadsafeMessageWriter
package object network {
	
	def mapToProtocol(convert:WormholeMap):GameProto.Map = {
		val mapBuild = GameProto.Map.newBuilder()
		mapBuild.setWidth(convert.width)
		mapBuild.setHeight(convert.height)
		val playerList = convert.players
		convert.objects foreach {
			obj =>
				val dataFuture = obj.dataFuture
				lazy val data = fetch(dataFuture)
				val ownerFuture = obj.ownerFuture
				lazy val owner = fetch(ownerFuture)
				
				val objBuild = GameProto.MapGridSpace.newBuilder()
				objBuild.setX(data.location.x)
				objBuild.setY(data.location.y)
				objBuild.setProductivity(data.productivity)
				objBuild.setDefense(data.defense)
				owner foreach {own => objBuild.setOwnerId(own)}
				objBuild.addAllUnitInfo(unitMapToProtocol(obj.units))
				mapBuild.addSpace(objBuild.build())
		}
		playerList foreach {
			player =>
				val builder = GameProto.Player.newBuilder()
				builder.setId(player.id)
				builder.setColor(player.color.getRGB())
				mapBuild.addPlayer(builder.build())
		}
		mapBuild.build()
	}

	def unitMapToProtocol(units:Map[PlayerId, Int]):ListBuffer[GameProto.UnitInfo] = {
		val lb = new ListBuffer[GameProto.UnitInfo]() 
		lb ++= units.map {
			tup =>
				val build = GameProto.UnitInfo.newBuilder()
				build.setOwner(tup._1)
				build.setCount(tup._2)
				build.build()
		}
		lb
	}
	
	def protocolToMap(convert:GameProto.Map, conn:ThreadsafeMessageWriter = null):WormholeMap = {
		val w = convert.getWidth()
		val h = convert.getHeight()
		val players:List[Player] = convert.getPlayerList() map {
			playInf =>
				new Player(playInf.getId(), new Color(playInf.getColor()))
		} toList
		val gameMap = new WormholeMap(w,h,players)
		convert.getSpaceList() foreach {
			space =>
				val x = space getX()
				val y = space getY()
				val prod = space getProductivity()
				val defense = space getDefense()
				val planet = new BaseObject(x,y,prod,defense,gameMap, new PlanetSprite(_), conn)
				if(space.hasOwnerId()){
					players find {_.id == space.getOwnerId()} map {_.id} foreach {o => planet.setOwner(o)}
				}
				val unitMap = unitInfoListProtoToBaseList(space.getUnitInfoList(), players)
				planet.setAllUnits(unitMap)
				gameMap.addObject(planet)
		}
		gameMap
	}
	
	def unitInfoListProtoToBaseList(list:Iterable[GameProto.UnitInfo], playerList:List[Player]):Map[PlayerId,Int] = {
		var map = Map[PlayerId, Int]()
		list foreach{
			inf =>
				val owner = playerList find {_.id==inf.getOwner()}
				owner foreach {
					own =>
						map += ((own.id, inf.getCount()))
				}
		}
		map
	}
}