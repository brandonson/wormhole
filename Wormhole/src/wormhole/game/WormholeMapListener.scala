package wormhole.game

import wormhole.PlayerId

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
	
	/**
	 * Called when a player wins the game.  Victory occurs when all planets are owned by one player, or are neutral.
	 */
	def playerVictory(map:WormholeMap, winner:PlayerId)
}