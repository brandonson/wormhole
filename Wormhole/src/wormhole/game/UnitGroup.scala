package wormhole.game

import wormhole.actor._
import wormhole.graphics.MapDisplay.GRID_SIDE
import akka.actor.Actor
import wormhole.graphics.Sprite
import wormhole.WormholeSystem
import akka.actor.Props
import akka.pattern.ask
import wormhole.game.network.GameProto

class UnitGroup(val id:Int, val owner:PlayerId, val count:Int, spriteGen:(UnitGroup) => Sprite, val map:WormholeMap, from:Location, to:Location) extends Updateable{
	val sprite = if (spriteGen != null) spriteGen(this) else null
	
	private val ref = WormholeSystem.actorOf(Props(new UnitGroupImpl(from, to, this)))
	
	def update(){
		ref ! 'Update
	}
	
	def locationFuture = (ref ? 'Location).mapTo[Location]
	def location = fetch (locationFuture)
	def isCompleteFuture = (ref ? 'Complete).mapTo[Boolean]
	def isComplete = fetch(isCompleteFuture)
	def setLocation(loc:Location) = ref ! ('NewLoc, loc)

}

class UnitGroupImpl(val fromLocation:Location, val toLocation:Location, val main:UnitGroup) extends Actor{
	
	var lData:LineData = if(wormhole.clientConnection != null) new BasicData(fromLocation) else new LineComputation(fromLocation, toLocation)
	
	trait LineData{
		def curX:Int
		def curY:Int
		def update()
		def forLocation(loc:Location):LineData
		def complete:Boolean
	}
	class BasicData(val loc:Location) extends LineData{
		
		def curX = loc.x
		def curY = loc.y
		def update(){}
		def forLocation(loc:Location) = new BasicData(loc)
		def complete = false
	}
	class LineComputation(from:Location, to:Location) extends LineData{
		val start = Location(from.x*GRID_SIDE+GRID_SIDE/2, from.y*GRID_SIDE+GRID_SIDE/2)
		val end = Location(to.x*GRID_SIDE+GRID_SIDE/2, to.y*GRID_SIDE+GRID_SIDE/2)
		val distX = math.abs(end.x-start.x)
		val distY = math.abs(end.y-start.y)
		val stepX = if (start.x<end.x) 1 else -1
		val stepY = if (start.y<end.y) 1 else -1
		var err = distX-distY
		var currentX = start.x
		var currentY = start.y
		def curX = currentX
		def curY = currentY
		
		def complete = curX==end.x&&curY==end.y
		def update(){
			if (!complete){
				val err2 = err*2
				if(err2 > -distY){
					err -= distY
					currentX += stepX
				}
				if(err2<distX){
					err+=distX
					currentY += stepY
				}
				val mType = GameProto.IncomingMessageType.newBuilder().setType(GameProto.MessageType.UNIT_GROUP_POSITION).build()
				val builder = GameProto.UnitGroupPosition.newBuilder()
				builder.setId(main.id)
				if(complete){
					builder.setComplete(true)
				}else{
					builder.setX(currentX)
					builder.setY(currentY)
				}
				main.map.sendToAll(mType, builder.build())
			}
		}
		def forLocation(loc:Location) = new LineComputation(loc, to)
	}
	
	def receive = {
		case 'Location =>
			sender ! Location(lData.curX, lData.curY)
		case 'Update =>
			lData.update()
			if(lData.complete){
				main.map.objectAt(toLocation.x, toLocation.y) foreach {_.unitArrival(main.count, main.owner)}
				main.map.removeGroup(main.id)
			}
		case 'Complete =>
			sender ! lData.complete
		case ('NewLoc, loc:Location) =>
			lData = lData.forLocation(loc)
	}
}
