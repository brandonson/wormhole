package wormhole.lobby

import javax.swing.ListCellRenderer
import wormhole.lobby.network.LobbyProto
import javax.swing.JList
import javax.swing.JLabel
import java.awt.Color
import java.awt.Font

object PersonInfoRenderer{
	val DISPLAY_FONT = new Font("Arial", Font.PLAIN, 15)
}

class PersonInfoRenderer extends JLabel with ListCellRenderer[LobbyProto.PersonInfo]{
	
	def getListCellRendererComponent(list:JList[_<:LobbyProto.PersonInfo], obj:LobbyProto.PersonInfo, idx:Int, focus:Boolean, sel:Boolean) = {
		setText(obj.getName())
		setForeground(new Color(obj.getColor()))
		setFont(PersonInfoRenderer.DISPLAY_FONT)
		this
	}
}