package wormhole.game
import scala.util.Random
import scala.collection.mutable.ListBuffer
import wormhole.graphics.PlanetSprite
import wormhole.Player

/**
 * Utility functions for generating maps.
 */
object MapUtils {

	/**
	 * Generates a random map with the given parameters, for the given players.  This map will contain only planets
	 * as BaseObjects
	 */
	def genRandomMap(width:Int, height:Int, planetCount:Int, maxProd:Int, maxDef:Int, players:List[Player]):WormholeMap = {
		//FIXME throw exception if there are more planets than area in the map
		//create the map
		val map = new WormholeMap(width, height, players)
		
		//generate planets
		val planets = new ListBuffer[BaseObject]
		for(i <- 0 until planetCount){
			
			var x = Random.nextInt(width)
			var y = Random.nextInt(height)
			
			//new location if there already is one at the given position.
			while(map.objectAt(x, y) isDefined){
				//TODO remove possibility of a long loop
				//this will take a long time if there are almost as many planets as there
				//are spaces on the map, see if there are better ways to do this  
				x = Random.nextInt(width)
				y = Random.nextInt(height)
			}
			
			//generate data
			val prod = Random.nextInt(maxProd)+1
			val defense = Random.nextInt(maxDef)+1
			val planet = new BaseObject(x,y,prod, defense, map, new PlanetSprite(_))
			
			//add to map and planet list
			map.addObject(planet)
			planets += planet
		}
		//give each player a planet
		players foreach {p => planets.remove(Random.nextInt(planets size)).setOwner(p.id)}
		map
	}
}