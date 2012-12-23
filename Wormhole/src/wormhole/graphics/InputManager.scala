package wormhole.graphics
import java.awt.event.KeyEvent
import java.awt.KeyEventPostProcessor
import java.awt.KeyboardFocusManager
import java.awt.event.MouseEvent
import javax.swing.event.MouseInputListener
import org.slf4j.LoggerFactory
/**
 * Handles input data.  The various fetch methods perform resets to data, and should all
 * be called in one loop of input handling.
 */
/*
 * TODO represent data as a single object.  Currently, the fetch methods
 * reset only the data they fetch.  Instead, data should be grabbed all at once.
 */
object InputManager extends KeyEventPostProcessor with MouseInputListener{

	KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventPostProcessor(InputManager)
  
	private val log = LoggerFactory.getLogger("InputManager")
  
	private var pressed:List[Int] = Nil
	
	private var down:List[Int] = Nil
	
	private var mousePos:(Int, Int) = (0,0)
	
	private var dragAmount:(Int,Int) = (0,0)
	private var mbDown:Option[Int] = None
	private var mouseButton:Option[Int] = None
	private var mbClickLocation:Option[(Int,Int)] = None
	
	def clear(){
		log.info("Clearing manager")
		pressed = Nil
	
		down = Nil
	
		mousePos = (0,0)
	
		dragAmount = (0,0)
		mbDown = None
		mouseButton = None
		mbClickLocation = None
	}
	
	def postProcessKeyEvent(keyEvt:KeyEvent):Boolean = {
		log.debug("Received KeyEvent")
		if(keyEvt.getID()==KeyEvent.KEY_PRESSED){
			if(!pressed.contains(keyEvt.getKeyCode())){
				pressed ::= keyEvt.getKeyCode()
				log.debug("Added key " + keyEvt.getKeyCode() + " to pressed list")
			}
			down ::= keyEvt.getKeyCode()
			log.debug("Key " + keyEvt.getKeyCode() + " is down.")
		}else if (keyEvt.getID()==KeyEvent.KEY_RELEASED){
			down = down filterNot {_==keyEvt.getKeyCode()}
			log.debug("Key " + keyEvt.getKeyCode() + " is up.")
		}
		true
	}
	
	def keyTyped(keyEvt:KeyEvent){}
	
	def fetchPressed() = {
		log.debug("Fetching pressed keys")
		val cp = pressed
		pressed = down
		cp
	}
	
	def mouseMoved(e:MouseEvent){
		log.debug("Mouse moved")
		mousePos = (e.getX(),e.getY())
	}
	def mouseDragged(e:MouseEvent){
		log.debug("Mouse dragged")
		dragAmount = (dragAmount._1 + mousePos._1-e.getX(), dragAmount._2 + mousePos._2-e.getY())
		mousePos = (e.getX(),e.getY())
	}
	def mouseClicked(e:MouseEvent){}
	def mousePressed(e:MouseEvent){
		log.debug("Mouse pressed")
		mbDown = Some(e.getButton())
		mbClickLocation = Some((e.getX(), e.getY()))
		mouseButton = mbDown
	}
	def mouseReleased(e:MouseEvent){
		log.debug("Mouse released")
		mbDown = None
	}
	
	def mouseExited(e:MouseEvent){}
	def mouseEntered(e:MouseEvent){}
	
	def fetchDragAmount = {
		log.debug("Fetching drag amount: " + dragAmount)
		val res = dragAmount
		dragAmount = (0,0)
		res
	}
	def fetchMousePosition = mousePos
	def fetchClickLocation = {
		log.debug("Fetching click location: " + mbClickLocation)
		val res = mbClickLocation
		mbClickLocation = None
		res
	}
	def fetchMouseButton = {
		log.debug("Fetching mouse button: " + mouseButton)
		val res = mouseButton
		mouseButton = mbDown
		res
	}
}