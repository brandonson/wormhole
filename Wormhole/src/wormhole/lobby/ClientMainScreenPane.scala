package wormhole.lobby

import javax.swing._
import wormhole.lobby.network.MainScreenProto
import javax.swing.DefaultListCellRenderer
import java.awt.event.ActionEvent
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import java.awt.Dimension

class ClientMainScreenPane(val conn:WormholeMainClient) extends JPanel{

	val lobbyList = new JList(conn.model)
	val scroll = new JScrollPane(lobbyList)
	scroll.setMinimumSize(new Dimension(200,400))
	scroll.setPreferredSize(scroll.getMinimumSize())
	scroll.setMaximumSize(new Dimension(500,1000))
	lobbyList.setCellRenderer(ListRenderer)
	lobbyList.addListSelectionListener(new ListSelectionListener{
		def valueChanged(e:ListSelectionEvent){
			roomName.setText(lobbyList.getSelectedValue().getName())
		}
	})
	lobbyList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
	
	val roomName = new JTextField(30)
	val buttonPanel = new JPanel()
	val joinButton = new JButton(new AbstractAction("Join"){
		def actionPerformed(e:ActionEvent){
			val selected = lobbyList.getSelectedValue()
			conn.joinLobby(selected)
		}
	})
	val createButton = new JButton(new AbstractAction("Create"){
		def actionPerformed(e:ActionEvent){
			val text = roomName.getText()
			if(!text.isEmpty()){
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