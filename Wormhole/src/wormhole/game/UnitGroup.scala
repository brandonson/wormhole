package wormhole.game

import wormhole.actor._
import wormhole.graphics.MapDisplay.GRID_SIDE
import akka.actor.Actor
import wormhole.graphics.Sprite
import wormhole.WormholeSystem
import akka.actor.Props
import akka.pattern.ask

class UnitGroup(val owner:PlayerId, val count:Int, spriteGen:(UnitGroup) => Sprite, from:BaseObject, to:BaseObject) extends Updateable{
	val sprite = spriteGen(this)
	
	private val ref = WormholeSystem.actorOf(Props(new UnitGroupImpl(from, to, this)))
	
	def update(){
		ref ! 'Update
	}
	
	def locationFuture = (ref ? 'Location).mapTo[Location]
	def location = fetch (locationFuture)
	def isCompleteFuture = (ref ? 'Complete).mapTo[Boolean]
	def isComplete = fetch(isCompleteFuture)
}

class UnitGroupImpl(val from:BaseObject, val to:BaseObject, val main:UnitGroup) extends Actor{
	
	val fromDataFuture = from.dataFuture
	lazy val fromData = fetch(fromDataFuture)

	val toDataFuture = to.dataFuture
	lazy val toData = fetch(toDataFuture)
	
	object LineComputation {
		val start = Location(fromData.location.x*GRID_SIDE+GRID_SIDE/2, fromData.location.y*GRID_SIDE+GRID_SIDE/2)
		val end = Location(toData.location.x*GRID_SIDE+GRID_SIDE/2, toData.location.y*GRID_SIDE+GRID_SIDE/2)
		val distX = math.abs(end.x-start.x)
		val distY = math.abs(end.y-start.y)
		val stepX = if (start.x<end.x) 1 else -1
		val stepY = if (start.y<end.y) 1 else -1
		var err = distX-distY
		
		var curX = start.x
		var curY = start.y
		
		def isComplete = curX==end.x&&curY==end.y
		def step(){
			if (!isComplete){
				val err2 = err*2
				if(err2 > -distY){
					err -= distY
					curX += stepX
				}
				if(err2<distX){
					err+=distX
					curY += stepY
				}
			}
		}
	}
	
	def receive = {
		case 'Location =>
			sender ! Location(LineComputation.curX, LineComputation.curY)
		case 'Update =>
			LineComputation.step()
			if(LineComputation.isComplete){
				to.unitArrival(main.count, main.owner)
			}
		case 'Complete =>
			sender ! LineComputation.isComplete
	}
}
