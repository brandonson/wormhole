package wormhole.lobby

import wormhole.SocketInfoData
import wormhole.lobby.network.MainScreenProto
import wormhole.lobby.network.MainScreenProto.MessageType._
import javax.swing.DefaultListModel
import scala.collection.JavaConversions._
import javax.swing.JOptionPane
import javax.swing.JFrame
import java.util.Arrays

/**
 * Client class for the main screen.
 */
class WormholeMainClient(val socket:SocketInfoData) extends Runnable{

	def in = socket.in
	def out = socket.out

	val model = new DefaultListModel[MainScreenProto.LobbyData]()
	
	private[this] var continue = true;
	
	/**
	 * Read list of lobbies on the server.
	 */
	private[this] def readLobbyList(){
		val listMsg = MainScreenProto.LobbyDataList.parseDelimitedFrom(in)
		val list = listMsg.getLobbyList()
		list foreach {model.addElement(_)}
	}
	
	def run(){
		readLobbyList()
		val frame = new JFrame("Wormhole")
		val cmsp = new ClientMainScreenPane(this)
		frame.setContentPane(cmsp)
		frame.pack()
		frame.setVisible(true)
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		while(continue && !Thread.interrupted()){
			val mType = MainScreenProto.MainMessageType.parseDelimitedFrom(in)
			mType.getType() match{
				case JOIN_LOBBY =>
					new Thread(new WormholeLobbyClient(socket), "Lobby Client").start()
					continue = false
				case NEW_LOBBY =>
					println("joinlobby")
					val lobbyMsg = MainScreenProto.LobbyData.parseDelimitedFrom(in)
					model.addElement(lobbyMsg)
				case REMOVED_LOBBY =>
					val lobbyId = MainScreenProto.LobbyIdMessage.parseDelimitedFrom(in)
					val id = lobbyId.getLobbyId()
					var modelIdx = -1
					for(i <- 0 until model.getSize()){
						if(model.getElementAt(i).getId() == id && modelIdx == -1){
							modelIdx = i
						}
					}
					if(modelIdx != -1){
						model.removeElementAt(modelIdx)
					}
				case LOBBY_LIST =>
					model.removeAllElements()
					readLobbyList()
				case DISCONNECT =>
					handleDisconnect()
					continue = false
			}
		}
		frame.setVisible(false)
	}
	
	private[this] def handleDisconnect(){
		JOptionPane.showMessageDialog(null, "Server ordered disconnect.")
		System.exit(0)
	}
	def createLobby(name:String){
		val typeMsg = MainScreenProto.MainMessageType.newBuilder().setType(CREATE_LOBBY).build()
		val createMsg = MainScreenProto.CreateLobby.newBuilder().setName(name).build()
		out.write(typeMsg, createMsg)
	}
	def joinLobby(lobbyData:MainScreenProto.LobbyData){
		val typeMsg = MainScreenProto.MainMessageType.newBuilder().setType(JOIN_LOBBY).build()
		val joinMsg = MainScreenProto.LobbyIdMessage.newBuilder().setLobbyId(lobbyData.getId()).build()
		out.write(typeMsg, joinMsg)
	}
	def disconnect(){
		val typeMsg = MainScreenProto.MainMessageType.newBuilder().setType(DISCONNECT).build()
		out.write(typeMsg)
	}
}