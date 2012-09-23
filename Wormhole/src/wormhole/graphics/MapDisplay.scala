package wormhole.graphics

import java.awt.image.BufferStrategy
import wormhole.game.WormholeMap
import javax.swing.JFrame
import wormhole.game.Player
import wormhole.actor._
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Font
object MapDisplay{
	val GRID_SIDE = 50
	val DATA_FONT = new Font("Arial", Font.PLAIN, 12)
}

/**
 * Displays a wormhole map using active rendering.  The display does not do rendering itself,
 * instead, a loop must call the render method.
 */
class MapDisplay(map:WormholeMap) extends JFrame {

	import MapDisplay.GRID_SIDE
	
	setIgnoreRepaint(true)
	addMouseListener(InputManager)
	addMouseMotionListener(InputManager)
	var buffer:BufferStrategy = null
	
	var renderDisplace = (0,0)
	private def setupBuffer(){
		this.createBufferStrategy(2)
		buffer = this.getBufferStrategy()
	}
	
	def render(){
		if(buffer==null){
			setupBuffer()
		}
		do{
			do{
				val g = buffer.getDrawGraphics().asInstanceOf[Graphics2D]
				//fill background
				g.setColor(Color.BLACK)
				g.fillRect(0, 0, getWidth(), getHeight())
				
				//get futures for all sprites
				val futures = map.objects map {o =>(o.spriteFuture, o.dataFuture)}
				val unitGroupFutures = map.unitGroups map {grp => (grp, grp.locationFuture)}
				
				//render bases
				futures foreach {
					tuple =>
						val (spriteFuture, dataFuture) = tuple
						val sprite = fetch(spriteFuture)
						val location = fetch(dataFuture).location
						sprite.render(g, location.x*GRID_SIDE+renderDisplace._1, location.y*GRID_SIDE+renderDisplace._2)
				}
				//render unit groups
				unitGroupFutures foreach {
					future =>
						val group = future._1
						val loc = fetch(future._2)
						group.sprite.render(g, loc.x + renderDisplace._1, loc.y + renderDisplace._2)
				}
			}while(buffer.contentsRestored())
			buffer show
		}while(buffer.contentsLost())
	}
	
	/**
	 * Convert coordinates of some event on this display to coordinates on the map it is displaying.
	 */
	def getMapXY(dispX:Int, dispY:Int):(Int,Int) = ((dispX-renderDisplace._1)/GRID_SIDE, (dispY-renderDisplace._2)/GRID_SIDE)
}
