package wormhole.game
import scala.util.Random
import scala.collection.mutable.ListBuffer
import wormhole.graphics.PlanetSprite


object MapUtils {

	def genRandomMap(width:Int, height:Int, planetCount:Int, maxProd:Int, maxDef:Int, players:List[Player]):WormholeMap = {
		val map = new WormholeMap(width, height, players)
		val planets = new ListBuffer[BaseObject]
		for(i <- 0 until planetCount){
			
			var x = Random.nextInt(width)
			var y = Random.nextInt(height)
			while(map.objectAt(x, y) isDefined){
				x = Random.nextInt(width)
				y = Random.nextInt(height)
			}
			val prod = Random.nextInt(maxProd)+1
			val defense = Random.nextInt(maxDef)+1
			val planet = new BaseObject(x,y,prod, defense, map, new PlanetSprite(_))
			map.addObject(planet)
			planets += planet
		}
		players foreach {p => planets.remove(Random.nextInt(planets size)).setOwner(p.id)}
		map
	}
}