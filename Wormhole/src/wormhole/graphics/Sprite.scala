package wormhole.graphics

import java.awt.Graphics2D

/**
 * Represents sprites to be drawn to the screen.
 */
trait Sprite {

	/**
	 * Renders this sprite at the given x and y coordinates using the graphics object.
	 */
	def render(g:Graphics2D, x:Int, y:Int)
}