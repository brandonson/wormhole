package wormhole.graphics
import java.awt.event.KeyEvent
import java.awt.KeyEventPostProcessor
import java.awt.KeyboardFocusManager
import java.awt.event.MouseEvent
import javax.swing.event.MouseInputListener

object InputManager extends KeyEventPostProcessor with MouseInputListener{

	private var pressed:List[Int] = Nil
	
	private var down:List[Int] = Nil
	
	private var mousePos:(Int, Int) = (0,0)
	
	private var dragAmount:(Int,Int) = (0,0)
	private var mbDown:Option[Int] = None
	private var mouseButton:Option[Int] = None
	private var mbClickLocation:Option[(Int,Int)] = None
	
	def postProcessKeyEvent(keyEvt:KeyEvent):Boolean = {
		if(keyEvt.getID()==KeyEvent.KEY_PRESSED){
			if(!pressed.contains(keyEvt.getKeyCode())){
				pressed ::= keyEvt.getKeyCode()
			}
			down ::= keyEvt.getKeyCode()
		}else if (keyEvt.getID()==KeyEvent.KEY_RELEASED){
			down -= keyEvt.getKeyCode()
		}
		true
	}
	
	def keyTyped(keyEvt:KeyEvent){}
	
	def fetchPressed() = {
		val cp = pressed
		pressed = down
		cp
	}
	
	KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventPostProcessor(InputManager)
	
	def mouseMoved(e:MouseEvent){
		mousePos = (e.getX(),e.getY())
	}
	def mouseDragged(e:MouseEvent){
		dragAmount = (dragAmount._1 + mousePos._1-e.getX(), dragAmount._2 + mousePos._2-e.getY())
		mousePos = (e.getX(),e.getY())
	}
	def mouseClicked(e:MouseEvent){}
	def mousePressed(e:MouseEvent){
		mbDown = Some(e.getButton())
		mbClickLocation = Some((e.getX(), e.getY()))
		mouseButton = mbDown
	}
	def mouseReleased(e:MouseEvent){
		mbDown = None
	}
	
	def mouseExited(e:MouseEvent){}
	def mouseEntered(e:MouseEvent){}
	
	def fetchDragAmount = {
		val res = dragAmount
		dragAmount = (0,0)
		res
	}
	def fetchMousePosition = mousePos
	def fetchClickLocation = {
		val res = mbClickLocation
		mbClickLocation = None
		res
	}
	def fetchMouseButton = {
		val res = mouseButton
		mouseButton = mbDown
		res
	}
}