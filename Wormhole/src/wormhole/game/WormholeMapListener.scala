package wormhole.game
/**
 * Trait for listeners to map changes and updates.
 */
trait WormholeMapListener {

	/**
	 * Called when a new unit group has been added to a map.
	 */
	def newUnitGroup(map:WormholeMap, group:UnitGroup)
	/**
	 * Called when an update is completed on a map.
	 */
	def updateComplete(map:WormholeMap)
}