package wormhole.game

/**
 * Listener trait for events occuring to BaseObjects.
 */
trait BaseObjectListener {

	/**
	 * Called when the owner of a BaseObject has changed.
	 */
	def ownerChanged(player:PlayerId, obj:BaseObject)
	/**
	 * Called when units where added to a BaseObject.
	 */
	def unitsChanged(player:PlayerId, amount:Int, obj:BaseObject)
	/**
	 * Called when the number of units for all players at a BaseObject
	 * has changed.
	 */
	def allUnitsChanged(obj:BaseObject)
}