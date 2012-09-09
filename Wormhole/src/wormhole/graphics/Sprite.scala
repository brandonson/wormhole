package wormhole.graphics

import java.awt.Graphics2D

trait Sprite {

	def render(g:Graphics2D, x:Int, y:Int)
}