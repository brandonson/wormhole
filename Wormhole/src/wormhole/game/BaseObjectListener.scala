package wormhole.game

trait BaseObjectListener {

	def ownerChanged(player:PlayerId, obj:BaseObject)
	def unitsChanged(player:PlayerId, amount:Int, obj:BaseObject)
	def allUnitsChanged(obj:BaseObject)
}