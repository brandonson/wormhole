package wormhole.game

import wormhole.graphics.InputManager
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent
import java.util.Timer
import java.util.TimerTask
import java.net.Socket
import wormhole.game.network.ClientPlayerConnection
import wormhole.RunnableConversion._
import wormhole.SocketInfoData
import wormhole.graphics.MapDisplay

class WormholeGameClient(val data:SocketInfoData) {
	val connection = new ClientPlayerConnection(data, () => new Thread(continue _).start())
	
	def start(){new Thread(connection, "CPC").start()}
	
	def continue{
		val map = connection.map
		val display = new MapDisplay(map)
		//val genv = GraphicsEnvironment.getLocalGraphicsEnvironment()
		//genv.getDefaultScreenDevice().setFullScreenWindow(display)
		display.setLocation(500,0)
		display.setSize(400, 400)
		display.setVisible(true)
		val timer = new Timer()
		timer.scheduleAtFixedRate(new TimerTask{def run(){map.updateAll()}}, 1000, 1000)
		var selected:Option[BaseObject] = None
 		var continue = true
 		InputManager.clear()
		do{
			display.render()
			val pressed = InputManager.fetchPressed()
			if(pressed.contains(KeyEvent.VK_ESCAPE) || !connection.isRunning){
				continue = false;
			}else if(pressed.contains(KeyEvent.VK_F1)){
				continue = false
				connection.leaveGame()
			}else{
			val clickLoc = InputManager.fetchClickLocation
			InputManager.fetchMouseButton match{
				case Some(MouseEvent.BUTTON3) =>
					val (xdisp, ydisp) = display.renderDisplace
					val (movx, movy) = InputManager.fetchDragAmount
					display.renderDisplace = (xdisp-movx*3, ydisp - movy*3)
				case Some(MouseEvent.BUTTON1) =>
					clickLoc foreach {tup =>
						val (xClick, yClick) = display.getMapXY(tup._1, tup._2)
						if(xClick<map.width&&xClick>=0&&yClick<map.height&&yClick>=0){
							val baseOpt = map.objectAt(xClick, yClick)
							selected match {
								case Some(selectedBase) =>
									baseOpt match {
										case Some(base) if selectedBase != base =>
											selectedBase.attack(base)
										case _ =>
									}
									selected = None
								case _ =>
									selected = baseOpt
							}
						}
					}
					InputManager.fetchDragAmount //and ignore
				case b =>
			}
			Thread.sleep(50)
			}
		}while(continue)
		display.setVisible(false)
		display.dispose()
	}
}