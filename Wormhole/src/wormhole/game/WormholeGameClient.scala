package wormhole.game

import wormhole.graphics.InputManager
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent
import java.util.Timer
import java.util.TimerTask
import java.net.Socket
import wormhole.game.network.ClientPlayerConnection
import wormhole.RunnableConversion.methodToRunnable
import wormhole.SocketInfoData
import wormhole.graphics.MapDisplay
import org.slf4j.LoggerFactory

/**
 * Client for Wormhole games.
 */
class WormholeGameClient(val data:SocketInfoData) {
	val log = LoggerFactory.getLogger("WormholeGameClient")
	val connection = new ClientPlayerConnection(data, () => new Thread(continue _).start())
	
	def start(){
	  log.debug("Starting game client")
	  new Thread(connection, "CPC").start()
	}
	
	def continue{
		log.trace("Continue callback reached for game client")
		val map = connection.map
		
		//set up display
		val display = new MapDisplay(map)
		
		//Play in full-screen mode.  Currently disabled
		//val genv = GraphicsEnvironment.getLocalGraphicsEnvironment()
		//genv.getDefaultScreenDevice().setFullScreenWindow(display)
		
		display.setLocation(500,0)
		display.setSize(400, 400)
		display.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
		display.setVisible(true)
		
		log.debug("Display created and visible")
		
		var selected:Option[BaseObject] = None
 		var continue = true
 		
 		//Clear data from previous games from the InputManager
 		InputManager.clear()
		log.debug("Beginning game loop")
 		do{
			display.render()
			
			//handle input
			val pressed = InputManager.fetchPressed()
			if(pressed.contains(KeyEvent.VK_ESCAPE) || !connection.isRunning){
			  log.info("Game exited")
				//exit game
				continue = false
				connection.leaveGame()
			}else{
				val clickLoc = InputManager.fetchClickLocation
				InputManager.fetchMouseButton match{
					case Some(MouseEvent.BUTTON3) =>
					  	log.debug("Right click: adjusting view")
						//right mouse button, move map view
						val (xdisp, ydisp) = display.renderDisplace
						val (movx, movy) = InputManager.fetchDragAmount
						display.renderDisplace = (xdisp-movx*3, ydisp - movy*3)
					case Some(MouseEvent.BUTTON1) =>
					  	log.debug("Left click")
						//left mouse button, do selection or attack
						clickLoc foreach {tup =>
							//determine where on the map the click was
							val (xClick, yClick) = display.getMapXY(tup._1, tup._2)
							
							//if map location is valid
							if(xClick<map.width&&xClick>=0&&yClick<map.height&&yClick>=0){
								log.debug("Handling select/deselect")
								val baseOpt = map.objectAt(xClick, yClick)
								selected match {
									case Some(selectedBase) =>
										baseOpt match {
											case Some(base) if selectedBase != base =>
											  	log.debug("Selection made, launching attack")
												//if there is a selection, and a base was clicked, do an attack
												selectedBase.attack(base)
											case _ =>
											  	log.debug("Click with no target selection")
										}
										//reset selection to none
										selected = None
										log.debug("Selected base set to None")
									case _ =>
										log.debug(if(baseOpt.isDefined) "Selection set" else "Click with no target selection")
										//no previous selection, make new value the selection
										selected = baseOpt
								}
							}
						}
						//reset the drag amount value for the input manager
						InputManager.fetchDragAmount //and ignore
					case b =>
				}
				//don't overload the CPU by consistently checking drag amounts.
				//decreases the length of sleep time would increase responsiveness
				//but also increase CPU load, and currently this seems instantly responsive.
				Thread.sleep(50)
			}
		}while(continue)
		log.debug("Debug loop complete")
		//destroy display used
		display.setVisible(false)
		display.dispose()
		log.debug("View disposed: client is complete")
	}
}