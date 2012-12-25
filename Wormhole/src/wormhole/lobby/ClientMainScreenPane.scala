package wormhole.lobby

import javax.swing._
import wormhole.lobby.network.MainScreenProto
import javax.swing.DefaultListCellRenderer
import java.awt.event.ActionEvent
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import java.awt.Dimension
import org.slf4j.LoggerFactory

/**
 * Display for clients at the main screen.
 */
class ClientMainScreenPane(val conn:WormholeMainClient) extends JPanel{

	val log = LoggerFactory.getLogger("ClientMainScreen")
  
	/**
	 * The list of lobbies on the server
	 */
	val lobbyList = new JList(conn.model)
	val scroll = new JScrollPane(lobbyList)
	scroll.setMinimumSize(new Dimension(200,400))
	scroll.setPreferredSize(scroll.getMinimumSize())
	scroll.setMaximumSize(new Dimension(500,1000))
	
	//set up renderer for the lobbies
	lobbyList.setCellRenderer(ListRenderer)
	
	//Set up selection so that the text display changes to
	//The name of the selected room.
	lobbyList.addListSelectionListener(new ListSelectionListener{
		def valueChanged(e:ListSelectionEvent){
			roomName.setText(lobbyList.getSelectedValue().getName())
		}
	})
	//only one lobby can be selected at a time
	lobbyList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
	
	//set up display
	
	val roomName = new JTextField(30)
	val buttonPanel = new JPanel()
	val joinButton = new JButton(new AbstractAction("Join"){
		def actionPerformed(e:ActionEvent){
			log.info("Joining lobby")
			val selected = lobbyList.getSelectedValue()
			conn.joinLobby(selected)
		}
	})
	val createButton = new JButton(new AbstractAction("Create"){
		def actionPerformed(e:ActionEvent){
			val text = roomName.getText()
			if(!text.isEmpty()){
				log info "Creating lobby"
				conn.createLobby(text)
			}
		}
	})
	buttonPanel add joinButton
	buttonPanel add createButton
	
	this setLayout new BoxLayout(this, BoxLayout.Y_AXIS)
	this add scroll
	
	val rmNamePanel = new JPanel()
	rmNamePanel.add(roomName)
	this add rmNamePanel
	this add buttonPanel
}

object ListRenderer extends ListCellRenderer[MainScreenProto.LobbyData]{
	
	def getListCellRendererComponent(list:JList[_ <: MainScreenProto.LobbyData], data:MainScreenProto.LobbyData, idx:Int, selected:Boolean, focus:Boolean) = {
		val label = new JLabel(data.getName())
		if(selected){
			label.setBackground(list.getSelectionBackground())
			label.setForeground(list.getSelectionForeground())
			label.setOpaque(true)
		}
		label
	}
}