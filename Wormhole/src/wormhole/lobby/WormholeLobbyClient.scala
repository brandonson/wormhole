package wormhole.lobby

import java.net.Socket
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JLabel
import wormhole.lobby.network.LobbyProto
import wormhole.ThreadsafeMessageWriter
import javax.swing.JButton
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import wormhole.lobby.network.LobbyProto.MessageType._
import javax.swing.JOptionPane
import wormhole.SocketInfoData
import wormhole.game.WormholeGameClient
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import javax.swing.JRadioButton
import javax.swing.JList
import javax.swing.DefaultListModel
import javax.swing.BoxLayout
import java.awt.Color
import javax.swing.ButtonGroup
import java.awt.event.ActionListener
import java.awt.Dimension
import javax.swing.JTextField
import javax.swing.JScrollPane
import java.net.InetAddress
import java.net.UnknownHostException
/**
 * Client for game lobbies.
 */
class WormholeLobbyClient(val data:SocketInfoData) extends Runnable with ActionListener{
	
	private val BACK_COLOR = new Color(0,0,30)
	
	def in = data.in
	def out = data.out
	var colors:List[(JRadioButton, Int)] = Nil
	val model = new DefaultListModel[LobbyProto.PersonInfo]
	val playerDisplay = new JList(model)
	val frame = new JFrame("Wormhole Lobby")
	var ownInfo:LobbyProto.PersonInfo = null
	var continue = true
	
	val startButton = new JButton("Start")
	val leaveButton = new JButton("Leave")
	
	/**
	 * Create the display for the lobby.
	 */
	def createFrame(){
		val infoPanel = new JPanel()
		playerDisplay.setCellRenderer(new PersonInfoRenderer)
		val dim = new Dimension(150, math.max(100, (colors.headOption map {t =>t._1.getPreferredSize().height} getOrElse(15))*colors.length))
		playerDisplay.setMinimumSize(dim)
		playerDisplay.setPreferredSize(dim)
		val scroll = new JScrollPane(playerDisplay)
		scroll.setPreferredSize(new Dimension(playerDisplay.getPreferredSize().width+20, playerDisplay.getPreferredSize().height + 50))
		val ndim = new Dimension(dim.width+20, dim.height)
		infoPanel add scroll
		val buttons = new JPanel()
		buttons.setLayout(new BoxLayout(buttons, BoxLayout.Y_AXIS))
		colors foreach {buttons add _._1}
		infoPanel add buttons
		val nameText = new JTextField(15)
		val nameChangeButton = new JButton(new AbstractAction("Set Name"){
			def actionPerformed(evt:ActionEvent){
				if(nameText.getText().length()>0){
					ownInfo = LobbyProto.PersonInfo.newBuilder(ownInfo).setName(nameText.getText()).build()
					val mType = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
					out.write(mType, ownInfo)
					nameText.setText("")
				}
			}
		})
		val actionButtons = new JPanel()
		actionButtons.setLayout(new BoxLayout(actionButtons, BoxLayout.X_AXIS))
		actionButtons add startButton
		actionButtons.add(leaveButton)
		val namePanel = new JPanel()
		namePanel.add(nameText)
		namePanel.add(nameChangeButton)
		infoPanel add namePanel
		val main = new JPanel()
		main setLayout(new BoxLayout(main, BoxLayout.Y_AXIS))
		main add infoPanel
		main add namePanel
		main add actionButtons
		
		main setBackground BACK_COLOR
		infoPanel.setBackground(BACK_COLOR)
		namePanel setBackground(BACK_COLOR)
		actionButtons setBackground(BACK_COLOR)
		playerDisplay setBackground(BACK_COLOR)
		frame setContentPane(main)
		frame pack()
		frame.setVisible(true)
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	}
	
	def setupColors(){
		val colorSet = LobbyProto.PossibleColorList.parseDelimitedFrom(in)
		val group = new ButtonGroup()
		startButton.addActionListener(this)
		leaveButton.addActionListener(this)
		colors = (colorSet.getColorsList() map {
			colorInf =>
				val button = new JRadioButton()
				button.setBackground(new Color(colorInf.getColor()))
				button.addActionListener(this)
				group.add(button)
				(button, colorInf.getColor())
		}).toList
	}
	def setupOwnInfo(){
		ownInfo = LobbyProto.PersonInfo.parseDelimitedFrom(in)
		readPersonSetInfo()
		colors find {_._2==ownInfo.getColor()} foreach {_._1.setSelected(true)}
	}
	def getNameFromUser(){
		val name = JOptionPane.showInputDialog(null, "Enter name:", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
		if(name != null&& !name.isEmpty()){
			ownInfo = LobbyProto.PersonInfo.newBuilder(ownInfo).setName(name).build()
			val mType = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
			out.write(mType, ownInfo)
		}
	}
	def run(){
		//read initialization data from server
		setupColors()
		setupOwnInfo()
		
		//get a username from the user, send it to server
		getNameFromUser()
		
		//create frame to display and do loop
		createFrame()
		basicLoop()
	}
	
	def basicLoop(){
		while(continue){
			
			val lmt = LobbyProto.LobbyMessageType.parseDelimitedFrom(in).getType()
			lmt match{
				case NEW_PERSON =>
					val person = LobbyProto.PersonInfo.parseDelimitedFrom(in)
					model addElement person
					colors.find {_._2 == person.getColor()} foreach {_._1.setEnabled(false)}
				case LOST_PERSON =>
					val person = LobbyProto.PersonInfo.parseDelimitedFrom(in)
					colors.find {_._2 == person.getColor()} foreach {_._1.setEnabled(true)}
					model removeElement person
				case PERSON_SET_INFO =>
					readPersonSetInfo()
				case CHANGE_INFO =>
					val from = LobbyProto.PersonInfo.parseDelimitedFrom(in)
					val to = LobbyProto.PersonInfo.parseDelimitedFrom(in)
					val idx = model.indexOf(from)
					model.set(idx, to)
					if(from.getColor()!=to.getColor()){
						colors find {_._2 == from.getColor()} foreach {_._1.setEnabled(true)}
						colors find {_._2 == to.getColor()} foreach {_._1.setEnabled(false)}
					}
				case START =>
					disableActions()
					val msgOut = LobbyProto.LobbyMessageType.newBuilder().setType(START_CONFIRM).build()
					out.write(msgOut)
					new WormholeGameClient(data).start()
					continue = false
				case RETURN_TO_MAIN =>
					disableActions()
					val conf = LobbyProto.LobbyMessageType.newBuilder().setType(CONFIRM_RETURN_TO_MAIN).build()
					continue = false
					out.write(conf)
					new Thread(new WormholeMainClient(data)).start()
				case CONFIRM_RETURN_TO_MAIN =>
					frame.setVisible(false)
					new Thread(new WormholeMainClient(data)).start()
					continue = false
				case DISCONNECT =>
					out.close()
					JOptionPane.showMessageDialog(null, "Server disconnected.", "Wormhole Lobby", JOptionPane.PLAIN_MESSAGE)
					continue = false
			}
		}
		frame.setVisible(false)
	}
	
	/**
	 * Read list of people in lobby.
	 */
	def readPersonSetInfo(){
		model.removeAllElements()
		val buffer:ListBuffer[LobbyProto.PersonInfo] = new ListBuffer[LobbyProto.PersonInfo]()
		buffer ++= (LobbyProto.PersonSetInfo.parseDelimitedFrom(in).getInfoList())
		colors foreach {_._1.setEnabled(true)}
		buffer foreach {
			person => 
				model addElement person
				colors.find{_._2 == person.getColor()} foreach {_._1.setEnabled(false)}
		}
		colors find {_._2 == ownInfo.getColor()} foreach {
			tup =>
				val (button, _) = tup
				button setSelected true
				button setEnabled false
		}
	}
	
	def actionPerformed(e:ActionEvent){
		val button = e.getSource()
		if(button==startButton){
			val sMessage = LobbyProto.LobbyMessageType.newBuilder().setType(START)
			out.write(sMessage build)
		}else if(button==leaveButton){
			disableActions()
			val leave = LobbyProto.LobbyMessageType.newBuilder().setType(RETURN_TO_MAIN)
			out.write(leave build)
		}else{
			colors find {_._1==button} foreach {
				tup =>
					val color = tup._2
					ownInfo = LobbyProto.PersonInfo.newBuilder(ownInfo).setColor(color).build()
					val mType = LobbyProto.LobbyMessageType.newBuilder().setType(CHANGE_INFO).build()
					out.write(mType, ownInfo)
			}
		}
	}
	/**
	 * Disable all actions. Called when the lobby is left.
	 */
	private[this] def disableActions(){
		colors foreach {_._1.removeActionListener(this)}
		startButton.removeActionListener(this)
		leaveButton.removeActionListener(this)
	}
}