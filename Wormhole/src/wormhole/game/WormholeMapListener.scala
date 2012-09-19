package wormhole.game

trait WormholeMapListener {

	def newUnitGroup(map:WormholeMap, group:UnitGroup)
	def updateComplete(map:WormholeMap)
}