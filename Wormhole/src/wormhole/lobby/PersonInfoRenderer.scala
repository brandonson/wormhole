package wormhole.lobby

import javax.swing.ListCellRenderer
import wormhole.lobby.network.LobbyProto
import javax.swing.JList
import javax.swing.JLabel
import java.awt.Color
import java.awt.Font
import com.wormhole.network.PlayerProto
import wormhole.Player

object PersonInfoRenderer{
	val DISPLAY_FONT = new Font("Arial", Font.PLAIN, 15)
}

/**
 * Renderer for users in a lobby.
 */
class PersonInfoRenderer extends JLabel with ListCellRenderer[Player]{
	
	def getListCellRendererComponent(list:JList[_<:Player], obj:Player, idx:Int, focus:Boolean, sel:Boolean) = {
		setText(obj.name)
		setForeground(obj.color)
		setFont(PersonInfoRenderer.DISPLAY_FONT)
		this
	}
}