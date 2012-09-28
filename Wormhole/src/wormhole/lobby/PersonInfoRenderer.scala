package wormhole.lobby

import javax.swing.ListCellRenderer
import wormhole.lobby.network.LobbyProto
import javax.swing.JList
import javax.swing.JLabel
import java.awt.Color
import java.awt.Font
import com.wormhole.network.PlayerProto

object PersonInfoRenderer{
	val DISPLAY_FONT = new Font("Arial", Font.PLAIN, 15)
}

/**
 * Renderer for users in a lobby.
 */
class PersonInfoRenderer extends JLabel with ListCellRenderer[PlayerProto.Player]{
	
	def getListCellRendererComponent(list:JList[_<:PlayerProto.Player], obj:PlayerProto.Player, idx:Int, focus:Boolean, sel:Boolean) = {
		setText(obj.getName())
		setForeground(new Color(obj.getColor()))
		setFont(PersonInfoRenderer.DISPLAY_FONT)
		this
	}
}