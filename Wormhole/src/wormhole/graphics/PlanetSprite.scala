package wormhole.graphics
import wormhole.game.BaseObject
import wormhole.game.Player
import java.awt.Graphics2D
import java.awt.Color
import scala.collection.mutable.ListBuffer
import wormhole.actor._
class PlanetSprite(val baseObject:BaseObject) extends Sprite {
	
	def render(g:Graphics2D, x: Int, y: Int): Unit = {
		val dataFuture = baseObject.dataFuture
		lazy val data = fetch(dataFuture)
		val unitsFuture = baseObject.unitsFuture
		lazy val units = fetch(unitsFuture)
		val ownerFuture = baseObject.ownerFuture
		lazy val owner = fetch(ownerFuture)
		val map = baseObject.map
		lazy val player = owner flatMap {map.player(_)}
		g.setFont(MapDisplay.DATA_FONT)
		g.setColor(player map {_.color} getOrElse Color.GRAY)
		g.fillOval(x+15, y+15, 20, 20)
		g.setColor(Color.WHITE)
		val metrics = g.getFontMetrics()
		g.drawString("" + data.productivity, x, y+metrics.getHeight())
		g.drawString("" + data.defense, x, y+MapDisplay.GRID_SIDE)
		var mul = 1
		units.foreach {
			tup =>
				val (p,c) = tup
				map.player(p) foreach {
					play =>
						g.setColor(play.color)
						g.drawString("" + c, x+MapDisplay.GRID_SIDE/2, y+metrics.getHeight()*mul)
						mul += 1
				}
		}

	}

}