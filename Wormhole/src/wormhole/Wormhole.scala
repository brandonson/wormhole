package wormhole

import wormhole.lobby.WormholeLobbyClient
import java.net.Socket
import javax.swing.UIManager
import javax.swing.JOptionPane
import java.io.IOException
import wormhole.lobby.WormholeMainClient

object Wormhole {

	def main(args:Array[String]){
		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
		val in = JOptionPane.showInputDialog(null, "Server:", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
		if(in!=null){
			try{
				val data = new SocketInfoData(new Socket(in, CONNECTION_PORT))
				clientConnection = data.out
				new Thread(new WormholeMainClient(data)).start()
			}catch{
				case _:IOException =>
					JOptionPane.showMessageDialog(null, "Failed to connect.", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
					System.exit(0)
			}
		}
	}
}
